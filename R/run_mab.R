#------------------------------------------------------------------------------
#' @title Simulates MAB Trial From Prepared Inputs and Performs Inference
#' @name run_mab
#' @description Internal helper. Centralizes necessary functions to conduct a
#' a MAB trial with adaptive inference. It assumes all inputs have been preprocessed already
#' @inheritParams mab_from_rct
#' @inheritParams prep_rct_data
#' @inheritParams simulate_mab
#' @param starts  Numeric vector where element `i` is the starting row number of period `i`.
#' @param ends  Numeric vector where element `i` is the ending row number of period `i`.
#' @param imputation_information Object created by [precompute_imputation()] containing the conditional means and success dates
#' for each treatment block to impute from.
#' @param resimulation Logical flag; Whether or not this MAB Trial is being run as a re-simulated RCT, as opposed to an original simulation from specified
#' population parameters.
#' @param time_model_args Arguments passed to `time_model` function
#'
#' @inheritParams simulate_mab
#'
#'
#' @returns: A named list containing:
#' \itemize{
#' \item `final_data`: The processed `tibble` or `data.table`, containing new columns pertaining to the results of the trial.
#' \item `bandits`: A `tibble` or `data.table` containing the UCB1 values or Thompson Sampling posterior distributions for each period.
#' \item `assignment_probs`: A `tibble` or `data.table` containing the probability of being assigned each treatment arm at a given period.
#' \item `estimates`: A `tibble` or `data.table` containing all estimates of the means and variances related to the treatment arms.
#' \item `settings`: A named list of the configuration settings used in the trial.
#' }
#' @keywords internal
#'

run_mab <- function(
  data,
  resimulation,
  p = NULL,
  algorithm,
  control_augment,
  random_assign_prop,
  prior_periods,
  discount_rate,
  delayed_feedback,
  whole_experiment = NULL,
  conditions,
  blocking,
  clustering,
  data_cols,
  imputation_information = NULL,
  verbose,
  ndraws,
  starts,
  ends,
  keep_data = keep_data,
  r = r,
  time_model = NULL,
  time_model_args = NULL
) {
  verbose_log(verbose, "Starting Bandit Trial")
  periods <- length(starts)
  num_conditions <- length(conditions)

  sim_results <- mab_loop(
    data = data,
    resimulation = resimulation,
    algorithm = algorithm,
    control_augment = control_augment,
    random_assign_prop,
    prior_periods = prior_periods,
    discount_rate = discount_rate,
    whole_experiment = whole_experiment,
    delayed_feedback = delayed_feedback,
    num_conditions = num_conditions,
    conditions = conditions,
    blocking = blocking,
    clustering = clustering,
    data_cols = data_cols,
    imputation_information = imputation_information,
    verbose = verbose,
    ndraws = ndraws,
    starts = starts,
    ends = ends,
    periods = periods,
    p = p,
    time_model = time_model,
    time_model = time_model_args
  )

  verbose_log(verbose, "Computing final simulation estimates")

  iaipw_estimates <- compute_iaipw(
    data = sim_results[["final_data"]],
    assignment_probs = sim_results[["assignment_probs"]],
    conditions = conditions,
    periods = periods
  )
  aipw_estimates <- estimate_aipw(
    data = sim_results[["final_data"]],
    assignment_probs = sim_results[["assignment_probs"]],
    iaipw = iaipw_estimates,
    periods = periods,
    conditions = conditions,
    clustering = clustering,
    cluster_col = data_cols[["cluster_col"]]
  )

  ipw_estimates <- estimate_ipw(
    data = sim_results[["final_data"]],
    estimates = estimates,
    cluster_col = data_cols[["cluster_col"]],
    clustering = clustering,
    blocking = blocking,
    conditions = conditions
  )
  sample_estimates <- estimate_sample(
    data = sim_results[["final_data"]],
    conditions = conditions,
  )
  estimates <- combine_estimates(
    estimates = list(aipw_estimates, ipw_estimates[["ipw"]], sample_estimates),
    vcov = ipw_estimates[["vcov"]]
  )
  final_data <- if (keep_data || r == 1) sim_results[["final_data"]] else NULL

  results <- list(
    final_data = final_data,
    bandits = sim_results[["bandits"]],
    assignment_probs = sim_results[["assignment_probs"]],
    assignment_quantities = sim_results[["assignment_quantities"]],
    estimates = estimates[["est"]],
    ipw_vcov = estimates[["vcov"]],
    settings = NULL
  )
  return(results)
}

#' Runs Multi-Arm Bandit Trial
#' @name mab_loop
#'
#' @description Performs a full Multi-Arm Bandit (MAB) trial using Thompson Sampling or UCB1.
#' The function provides loop around each step of the process for each treatment wave, performing adaptive
#' treatment assignment, and outcome imputation. Supports flexible customizations in treatment blocking strategy,
#' stationary/non-stationary bandits, control augmentation, and hybrid assignment.
#'
#' @inheritParams mab_from_rct
#' @inheritParams prep_rct_data
#' @inheritParams simulate_mab
#' @param num_conditions Number of conditions, equivalent to `length(conditions)`.
#'
#'
#' @returns  A named list containing:
#' \itemize{
#' \item `final_data`: The processed `tibble` or `data.table`, containing new columns pertaining to the results of the trial.
#' \item `bandits`: A `tibble` or `data.table` containing the UCB1 values or Thompson Sampling posterior distributions for each period.
#' \item `assignment_probs`: A `tibble` or `data.table` containing the probability of being assigned each treatment arm at a given period.
#' }
#' @details
#' The first period is used to initialize the trial, so the MAB loop
#' starts at period number 2.
#'
#' @keywords internal
#'
mab_loop <- function(
  data,
  resimulation,
  p,
  algorithm,
  control_augment,
  random_assign_prop,
  prior_periods,
  discount_rate,
  whole_experiment = NULL,
  delayed_feedback,
  clustering,
  blocking,
  conditions,
  data_cols,
  imputation_information = NULL,
  ndraws,
  verbose,
  starts,
  ends,
  periods,
  num_conditions,
  time_model,
  time_model_args
) {
  bandits <- vector(mode = "list", length = 2)

  bandits[["bandit_stat"]] <- matrix(
    NA,
    nrow = periods,
    ncol = num_conditions,
    dimnames = list(c(), names(conditions))
  )
  bandits[["assignment_prob"]] <- matrix(
    NA,
    nrow = periods,
    ncol = num_conditions,
    dimnames = list(c(), names(conditions))
  )
  bandits[["assignment_prob"]][1, ] <- rep(
    1 / num_conditions,
    num_conditions
  )

  equal_probs <- bandits[["assignment_prob"]][1, ] |>
    as.numeric()
  names(equal_probs) <- conditions

  col_names <- lapply(data_cols, \(col) {
    col[["name"]]
  })

  for (i in 2:periods) {
    current_idx <- starts[i]:ends[i]
    verbose_log(verbose, paste0("Period: ", i))

    prior <- compute_lookback(prior_periods = prior_periods, current_period = i)

    current_data <- data[current_idx, ]
    prior_data <- data[starts[prior]:ends[i - 1], ]

    if (algorithm != "static") {
      current_bandit <- compute_prior(
        current_data = current_data,
        prior_data = prior_data,
        delayed_feedback = delayed_feedback,
        assignment_date_col = col_names[["assignment_date_col"]],
        conditions = conditions,
        discount_rate = discount_rate,
        current_period = i
      ) |>
        compute_bandit(
          algorithm = algorithm,
          num_conditions = num_conditions,
          conditions = conditions,
          current_period = i,
          control_augment = control_augment,
          ndraws = ndraws
        )
      bandits[["bandit_stat"]][i - 1, ] <- current_bandit[["bandit"]]
    } else {
      current_bandit[["assignment_prob"]] <- equal_probs
    }

    current_data <- assign_treatments(
      current_data = current_data,
      probs = current_bandit[["assignment_prob"]],
      blocking = blocking,
      clustering = clustering,
      cluster_col = col_names[["cluster_col"]],
      conditions_col = col_names[["conditions_col"]],
      conditions = conditions,
      random_assign_prop = random_assign_prop,
      random_probs = equal_probs,
      resimulation = resimulation
    )

    bandits[["assignment_probs"]][i, ] <- (current_bandit[["assignment_prob"]] *
      (1 - random_assign_prop)) +
      (equal_probs * random_assign_prop)

    if (resimulation) {
      prepped_impute <- prep_imputation(
        current_data = current_data,
        whole_experiment = whole_experiment,
        imputation_information = imputation_information,
        block_cols = col_names[["block_cols"]],
        clustering = clustering,
        blocking = blocking,
        delayed_feedback,
        current_period = i
      )
      data <- impute_outcomes(
        data = data,
        imputation_info = prepped_impute,
        success_col = col_names[["success_col"]],
        success_date_col = col_names[["success_date_col"]],
        delayed_feedback = delayed_feedback,
        idx = current_idx
      )
    } else {
      data <- generate_outcomes(
        current_data = current_data,
        data = data,
        p = p,
        idx = current_idx,
        time_model = time_model,
        time_model_args = time_model_args
      )
    }
  }
  results <- collect_mab_results(
    data = data,
    bandits = bandits,
    algorithm = algorithm,
    conditions = conditions,
    num_conditions = num_conditions,
    periods = periods,
    ndraws = ndraws
  )

  return(results)
}
#-------------------------------------------------------------------------------

#' @name collect_mab_results
#' @title Ends Multi-Arm Bandit Trial
#' @description Condenses output from [mab_loop()] into
#' manageable structure.
#' @param data Finalized data from [mab_loop()].
#' @param bandits Finalized bandits list of matrices from [mab_loop()].
#' @param periods Numeric value of length 1; total number of periods in Multi-Arm-Bandit trial.
#' @inheritParams mab_loop
#' @inheritParams run_mab
#' @returns  A named list containing:
#' \itemize{
#' \item `final_data`: The processed `tibble` or `data.table`, containing new columns pertaining to the results of the trial.
#' \item `bandits`: A `tibble` or `data.table` containing the UCB1 values or Thompson sampling posterior distributions for each period.
#' \item `assignment_probs`: A `tibble` or `data.table` containing the probability of being assigned each treatment arm at a given period.
#' }
#' @seealso
#' * [mab_loop()]
#' @keywords internal

collect_mab_results <- function(
  data,
  bandits,
  algorithm,
  periods,
  conditions,
  num_conditions,
  ndraws
) {
  UseMethod("collect_mab_results", data)
}
#-------------------------------------------------------------------------------
#
#' @method collect_mab_results `data.frame`
#' @inheritParams collect_mab_results
#' @title [collect_mab_results()] for `data.frame`s
#' @noRd
collect_mab_results.data.frame <- function(
  data,
  bandits,
  algorithm,
  periods,
  conditions,
  num_conditions,
  ndraws
) {
  final_summary <- data |>
    dplyr::group_by(mab_condition) |>
    dplyr::summarize(
      successes = sum(mab_success, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    as.list() |>
    finalize_prior_list()

  final_bandit <- compute_bandit(
    past_results = final_summary,
    algorithm = algorithm,
    num_conditions = num_conditions,
    conditions = conditions,
    current_period = (periods + 1),
    control_augment = 0,
    ndraws = ndraws
  )

  bandits[["bandit_stat"]][periods, ] <- final_bandit[["bandit"]]
  df_bandits <- lapply(bandits, \(x) {
    tibble::as_tibble(x) |>
      dplyr::mutate(period_number = dplyr::row_number())
  })

  assignment_quantities <- data |>
    dplyr::group_by(mab_condition) |>
    dplyr::count() |>
    as_named_vec(
      val = "n",
      name = "mab_condition"
    )

  if (length(assignment_quantities) < length(conditions)) {
    missing <- setdiff(
      conditions,
      names(assignment_quantities)
    )
    assignment_quantities[missing] <- 0
  }

  matrix_idx <- cbind(
    data[["period_number"]],
    match(data[["mab_condition"]], conditions)
  )
  data <- data |>
    dplyr::mutate(
      mab_assign_prob = bandits[["assignment_prob"]][matrix_idx],
      ipw_weights <- 1 / mab_assign_prob
    )

  return(list(
    final_data = data,
    bandits = df_bandits[["bandit_stat"]],
    assignment_probs = df_bandits[["assignment_prob"]],
    assignment_quantities = assignment_quantities
  ))
}
#-------------------------------------------------------------------------------

#' @method collect_mab_results `data.table`
#' @inheritParams collect_mab_results
#' @title [collect_mab_results()] for `data.table`s
#' @noRd
collect_mab_results.data.table <- function(
  data,
  bandits,
  algorithm,
  periods,
  conditions,
  num_conditions,
  ndraws
) {
  final_summary <- data[,
    .(
      successes = sum(mab_success, na.rm = TRUE),
      n = .N
    ),
    by = mab_condition
  ] |>
    as.list() |>
    finalize_prior_list()

  final_bandit <- compute_bandit(
    past_results = final_summary,
    algorithm = algorithm,
    num_conditions = num_conditions,
    conditions = conditions,
    current_period = (periods + 1),
    control_augment = 0,
    ndraws = ndraws
  )
  bandits[["bandit_stat"]][periods, ] <- final_bandit[["bandit"]]
  bandit_stats <- data.table::as.data.table(bandits[["bandit_stat"]])
  bandit_stats[, period_number := .I]

  assignment_probs <- data.table::as.data.table(bandits[["assignment_prob"]])
  assignment_probs[, period_number := .I]

  assignment_quantities <- data[, .(count = .N), by = mab_condition] |>
    as_named_vec(val = "count", name = "mab_condition")

  if (length(assignment_quantities) < length(conditions)) {
    missing <- setdiff(conditions, names(assignment_quantities))
    assignment_quantities[missing] <- 0
  }

  matrix_idx <- cbind(
    data[["period_number"]],
    match(data[["mab_condition"]], conditions)
  )
  assign_vec <- bandits[["assignment_prob"]][matrix_idx]
  data[, `:=`(
    mab_assign_prob = assign_vec,
    ipw_weights = 1 / assign_vec
  )]
  return(list(
    final_data = data,
    bandits = bandit_stats,
    assignment_probs = assignment_probs,
    assignment_quantities = assignment_quantities
  ))
}
#------------------------------------------------------------------------------
#' Create Prior Periods
#' @name compute_lookback
#' @description Used during [mab_loop()] to create a vector of prior periods dynamically based on the specified
#' number of prior periods.
#' @inheritParams mab_from_rct
#' @param current_period The current period of the simulation. Defined by loop structure inside [mab_loop()].
#' @returns Numeric value referring to the period index to look back from.
#' the results for the current treatment assignment period.
#'
#' @seealso
#' * [mab_loop()]
#' @keywords internal

compute_lookback <- function(prior_periods = NULL, current_period) {
  if (is.null(prior_periods)) {
    1
  } else {
    current_period - prior_periods
  }
}

#' @title
#' Running Multi-Arm Bandit Trial and Adaptive Inference
#' @name single_mab_simulation
#'
#' @description Performs a single Multi-Arm Bandit (MAB) trial using experimental data from
#' an original randomized controlled trial, and adaptive inference strategies as described in
#' Hadad et al. (2021). This function wraps around [run_mab_trial()] and [get_adaptive_aipw()],
#' completing the full MAB pipeline: treatment assignment, success imputation, and estimation.
#'
#'
#' @param data A data frame, tibble or data.table that provides the input data for the trial.
#' @param time_unit A string specifying the unit of time for assigning periods when 'assignment_method` is 'date'.
#'  Acceptable values are "Day", "Week", or "Month".
#' @param perfect_assignment Logical; if TRUE, assumes perfect information for treatment assignment
#'                           (i.e., all outcomes are observed regardless of the date).
#'                           If FALSE, hides outcomes not yet theoretically observed, based
#'                           on the dates treatments would have been assigned for each wave.
#' @param algorithm A string specifying the MAB algorithm to use. Options are "Thompson" or "UCB1".
#' @param period_length Numeric; length of each treatment period.
#' If assignment method is "Date", this refers to the
#' length of periods by your specified `time_unit` (i.e., if "Day", 10 would be 10 days).
#' If assignment methods is "Batch", this refers to the number of people in each batch.
#' @param prior_periods Numeric; number of previous periods to use in the treatment assignment model
#' or specify string "All" to use all previous periods.
#' @param whole_experiment Logical; if TRUE, uses all past experimental data for imputing outcomes.
#'                         If FALSE, uses only data available up to the current period.
#' @param conditions Named Character vector containing treatment conditions.
#' Control condition, must be named "Control" when 'control_augment' > 0.
#' @param data_cols Named Character vector containing the names of columns in data as strings:
#' \itemize{
#' \item `id_col`: Column in data, contains unique id as a key.
#' \item `success_col`: Column in data; Binary successes from original experiment.
#' \item `condition_col`: Column in data; Original Treatment condition for each observation.
#' \item `date_col`: Column in data, contains original date of event/trial; only necessary when assigning by 'Date'.
#' \item `month_col`: Column in data, contains month of treatment; only necessary when time_unit = 'Month'.
#' \item `success_date_col`: Column in data, contains original dates each success occurred; only necessary when 'perfect_assignment' = FALSE.
#' \item `assignment_date_col`: Column in data, contains original dates treatments are assigned to observations; only necessary when 'perfect_assignment' = FALSE.
#' Used to simulate imperfect information on part of researchers conducting an adaptive trial.
#' }
#' @param blocking Logical; Whether or not to use treatment blocking.
#' @param block_cols Character Vector of variables to block by.
#' @param assignment_method String; "Date", "Batch" or "Individual" to define the assignment into treatment waves.
#' @param control_augment Number \eqn{\in} \[0,1\]; Proportion of each wave guaranteed to get "Control" treatment.
#' Default is 0.
#' @param verbose Logical; Whether or not to print iteration number. FALSE by default.
#'
#' @return `mab` class object, which is named list containing:
#' \itemize{
#' \item `final_data`: The processed data with treatment assignments and imputed outcomes, labelled with "mab_" prefix.
#' \item `bandits`: Either the UCB1 statistics or Thompson Sampling posterior distributions.
#' \item `assignment_probs`: Probability of being assigned each treatment arm at a given period
#' \item `estimates`: AIPW (Augmented Inverse Probability Weighting) treatment effect estimates and variances.
#' \item `settings`: A list of the configuration settings used in the trial.
#' }
#'
#' @seealso
#' * [run_mab_trial()]
#' * [get_adaptive_aipw()]
#' * [check_args()]
#' * [mab_simulation()]
#' * [pre_mab_simulation()]
#' @example inst/examples/single_mab_simulation_example.R

#'
#' @export

single_mab_simulation <- function(data,
                                  assignment_method,
                                  algorithm,
                                  conditions,
                                  prior_periods,
                                  perfect_assignment,
                                  whole_experiment,
                                  blocking,
                                  data_cols,
                                  control_augment = 0,
                                  time_unit = NULL,
                                  period_length = NULL,
                                  block_cols = NULL,
                                  verbose = FALSE) {
  prepped <- pre_mab_simulation(
    data = data, assignment_method = assignment_method,
    algorithm = algorithm, conditions = conditions,
    prior_periods = prior_periods, perfect_assignment = perfect_assignment,
    whole_experiment = whole_experiment, blocking = blocking,
    block_cols = block_cols, data_cols = data_cols,
    control_augment = control_augment, time_unit = time_unit,
    period_length = period_length,
    verbose = verbose
  )


  # Simulating the MAB Trial
  results <- mab_simulation(
    data = prepped$data,
    time_unit = time_unit,
    period_length = period_length,
    prior_periods = prior_periods,
    algorithm = algorithm,
    whole_experiment = whole_experiment,
    perfect_assignment = perfect_assignment,
    conditions = conditions,
    blocking = blocking,
    block_cols = prepped$block_cols,
    data_cols = prepped$data_cols,
    verbose = verbose,
    assignment_method = assignment_method,
    control_augment = control_augment,
    imputation_information = prepped$imputation_information
  )
  results$settings$original_data <- data

  return(results)
}
#------------------------------------------------------------------------------
#' Conducts Multiple Multi-Arm Bandit Trials with Adaptive Inference in Parallel
#'
#' @name multiple_mab_simulation
#' @description Repeated Multi-Arm Bandit Simulations with the same settings in different
#' random states. Allows for parallel processing using [future::plan()] and [furrr::future_map()].
#'
#'
#' @inheritParams single_mab_simulation
#' @param verbose Logical; Toggles progress bar from [furrr::future_map()].
#' @param times Integer; number of simulations to conduct.
#' @param seeds Integer vector of `length(times)` containing valid seeds to define random state for each trial.
#' @param keep_data Logical; Whether or not to keep the final data from each trial. Recommended FALSE for large datasets
#' .
#' @returns `multiple.mab` class object, which is a named list containing:
#' \itemize{
#' \item `final_data_nest:` Data.frame containing a nested data.frame with the final data from each trial
#' \item `bandits:` Data.frame containing the Thompson/UCB1 statistics across all treatments, periods, and trials
#' \item `estimates:` Data.frame containing the AIPW statistics across all treatments, and trials
#' \item `settings`: A list of the configuration settings used in the trial.
#' }
#' @example inst/examples/multiple_mab_simulation_example.R
#' @seealso
#' * [run_mab_trial()]
#' * [get_adaptive_aipw()]
#' * [check_args()]
#' * [single_mab_simulation()]
#' * [mab_simulation()]
#' * [pre_mab_simulation()]
#' * [furrr::future_map()]
#' * [future::plan()]
#' @export

multiple_mab_simulation <- function(data,
                                    assignment_method,
                                    algorithm,
                                    conditions,
                                    prior_periods,
                                    perfect_assignment,
                                    whole_experiment,
                                    blocking,
                                    data_cols,
                                    times,
                                    seeds,
                                    control_augment = 0,
                                    time_unit = NULL,
                                    period_length = NULL,
                                    block_cols = NULL,
                                    verbose = FALSE,
                                    keep_data = FALSE) {
  if ((utils::object.size(data) / (1024^2) > 500)) {
    rlang::warn(c(
      "i" = "`furrr::future_map()` has a serialization limit of 500 MB. If your data
    is larger than that, you have to set the `options(\"future_globals.maxSize\")`
    manually to change it. This has been known to cause failures"
    ))
  }

  if (!is.numeric(times) || times < 1 || floor(times) != times) {
    rlang::abort(c("Argument 'times' must be an integer value greater than or equal to 1"),
      "x" = paste0("You Passed: ", times)
    )
  }
  if (!is.integer(seeds) || length(seeds) != times) {
    rlang::abort(c("Argument 'seeds' must be an integer vector of length equal to `times`. Please provide a valid vector.",
      "x" = sprintf("You passed a %s vector of length %d, while times is %d.", base::typeof(seeds), base::length(seeds), times),
      "i" = "Reccomended to use `sample.int()` to create proper vector"
    ))
  }
  if (!is.logical(keep_data) || is.na(keep_data)) {
    rlang::abort("Argument 'keep_data' must logical. Please enter `TRUE` or `FALSE`")
  }


  prepped <- pre_mab_simulation(
    data = data, assignment_method = assignment_method,
    algorithm = algorithm, conditions = conditions,
    prior_periods = prior_periods, perfect_assignment = perfect_assignment,
    whole_experiment = whole_experiment, blocking = blocking,
    block_cols = block_cols, data_cols = data_cols,
    control_augment = control_augment, time_unit = time_unit,
    period_length = period_length,
    verbose = verbose
  )
  verbose_log(verbose, "Starting Simulations")

  mabs <- furrr::future_map(seq_len(times), ~ {
    set.seed(seeds[.x])
    results <- mab_simulation(
      data = prepped$data,
      time_unit = time_unit,
      period_length = period_length,
      prior_periods = prior_periods,
      algorithm = algorithm,
      whole_experiment = whole_experiment,
      perfect_assignment = perfect_assignment,
      conditions = conditions,
      blocking = blocking,
      block_cols = prepped$block_cols,
      data_cols = prepped$data_cols,
      verbose = FALSE,
      assignment_method = assignment_method,
      control_augment = control_augment,
      imputation_information = prepped$imputation_information
    )
    if (!keep_data) {
      results <- results[names(results) != "final_data"]
    }
    results <- results[names(results) != "settings"]
  },
  .options = furrr::furrr_options(
    seed = TRUE,
    packages = c(
      "whatifbandit", "dplyr", "rlang",
      "tidyr", "bandit", "tibble", "lubridate",
      "purrr", "furrr", "randomizr", "data.table"
    )
  ),
  .progress = verbose
  )
  verbose_log(verbose, "Collating Results")
  results <- condense_results(
    data = data, keep_data = keep_data, mabs = mabs,
    times = times
  )

  results$settings <- base::list(
    original_data = data,
    assignment_method = assignment_method,
    control_augment = control_augment,
    time_unit = time_unit,
    perfect_assignment = perfect_assignment,
    algorithm = algorithm,
    period_length = period_length,
    prior_periods = prior_periods,
    whole_experiment = whole_experiment,
    conditions = conditions,
    blocking = blocking,
    block_cols = prepped$block_cols$name,
    trials = times,
    keep_data = keep_data
  )

  return(results)
}
#' @name condense_results
#' @title Condenses results into a list for [multiple_mab_simulation()]
#' @description
#' Takes the output from [furrr::future_map()] in [multiple_mab_simulation()]
#' and condenses it to return to the user
#' @inheritParams multiple_mab_simulation
#' @param mabs output from [furrr::future_map()] in [multiple_mab_simulation()]
#' @returns `multiple.mab` class object, which is a named list containing:
#' \itemize{
#' \item `final_data_nest:` Data.frame containing a nested data.frame with the final data from each trial
#' \item `bandits:` Data.frame containing the Thompson/UCB1 statistics across all treatments, periods, and trials
#' \item `estimates:` Data.frame containing the AIPW statistics across all treatments, and trials
#' \item `settings`: A list of the configuration settings used in the trial.
#' }

condense_results <- function(data, keep_data, mabs, times) {
  items <- c("bandits", "estimates", "assignment_probs")

  if (inherits(data, "data.table")) {
    results <- lapply(items, \(item) {
      all <- lapply(seq_len(times), function(i) mabs[[i]][[item]])
      result <- data.table::rbindlist(all, idcol = "trial")
      result[, trial := as.numeric(trial)]
      return(result)
    })
    names(results) <- items
    if (keep_data) {
      results$final_data_nest <- data.table::data.table(
        trial = base::seq_len(times),
        data = purrr::map(mabs, ~ .x$final_data)
      )
    } else {
      results$final_data_nest <- NULL
    }
  } else {
    results <- purrr::map(items, function(item) {
      result <- purrr::map(seq_len(times), function(i) mabs[[i]][[item]]) |>
        dplyr::bind_rows(.id = "trial") |>
        dplyr::mutate(trial = as.numeric(trial))
      return(result)
    })
    names(results) <- items

    if (keep_data) {
      results$final_data_nest <- tibble::tibble(
        trial = base::seq_len(times),
        data = purrr::map(mabs, ~ .x$final_data)
      )
    } else {
      results$final_data_nest <- NULL
    }
  }



  base::class(results) <- c("multiple.mab", class(results))
  return(results)
}

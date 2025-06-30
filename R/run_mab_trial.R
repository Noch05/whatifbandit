#' Runs Multi-Arm Bandit Trial
#' @name run_mab_trial
#'
#' @description Performs a full Multi-Arm Bandit (MAB) trial using Thompson Sampling or UCB1.
#' The function provides loop around each step of the process for each treatment wave. The function
#' performs adaptive treatment assignment based on prior performance, imputes potential outcomes based on
#' original experimental data from and Randomized Controlled Trial (RCT), and then uses that information to update the
#' assignment for the next wave until completed. Supports flexible customization in treatment blocking strategy,
#' the size of each treatment wave, and information availability to simulate both a real experiment, and non-stationary
#' bandit strategy.
#'
#' @inheritParams single_mab_simulation
#'
#'
#' @return  A named list containing:
#' \item{final_data}{The processed data with treatment assignments and imputed outcomes labelled with "mab_" prefix.}
#' \item{bandits}{Either the UCB1 statistics or Thompson Sampling posterior distributions.}
#'
#' @seealso
#' * [single_mab_simulation()]
#' * [create_cutoff()]
#' *[create_prior()]
#' *[mab_sample()]
#' *[assign_treatments()]
#' @export
#'
run_mab_trial <- function(data, time_unit, period_length = NULL,
                          prior_periods, algorithm,
                          whole_experiment, perfect_assignment, conditions,
                          blocking = FALSE, block_cols = NULL,
                          date_col, month_col = NULL,
                          id_col, condition_col,
                          success_col, success_date_col,
                          assignment_date_col, verbose, control_augment) {
  verbose_log(verbose, "Precomputing")

  imputation_information <- imputation_prep(
    data = data,
    whole_experiment = whole_experiment,
    success_col = {{ success_col }},
    success_date_col = {{ success_date_col }},
    perfect_assignment = perfect_assignment,
    condition_col = {{ condition_col }}
  )

  bandits <- vector(mode = "list", length = max(data$period_number))
  if (algorithm == "Thompson") {
    bandits[[1]] <- rlang::set_names(rep(1 / length(conditions), length(conditions)), conditions)
  } else if (algorithm == "UCB1") {
    bandits[[1]] <- tibble::tibble(mab_condition = conditions, ucb = rep(0, length(conditions)))
  } else {
    base::stop("Please specify algorithm: Thompson or UCB1")
  }

  priors <- lapply(base::seq_len(base::max(data$period_number)), \(x) {
    create_prior(prior_periods = prior_periods, current_period = x)
  })

  data <- data |>
    dplyr::arrange(period_number, {{ id_col }})


  verbose_log(verbose, "Starting Bandit Trial")
  for (i in 2:max(data$period_number)) {
    verbose_log(verbose, paste0("Period: ", i))

    prior <- priors[[i]]

    current_data <- data |>
      dplyr::filter(period_number == i)

    prior_data <- data |>
      dplyr::filter(period_number %in% prior)

    past_results <- get_past_results(
      current_data = current_data,
      prior_data = prior_data,
      perfect_assignment = perfect_assignment,
      success_date_col = {{ success_date_col }},
      assignment_date_col = {{ assignment_date_col }},
      conditions = conditions
    )


    bandit <- get_bandit(
      past_results = past_results,
      algorithm = algorithm,
      conditions = conditions,
      current_period = i,
      control_augment = control_augment
    )

    bandits[[i]] <- bandit

    current_data <- assign_treatments(
      current_data = current_data,
      bandit = bandit,
      blocking = blocking,
      algorithm = algorithm,
      id_col = {{ id_col }},
      conditions = conditions,
      condition_col = {{ condition_col }},
      success_col = {{ success_col }}
    )

    # Creating block for imputing
    if (blocking) {
      current_data <- current_data |>
        dplyr::mutate(
          impute_block = do.call(paste, c(dplyr::across(c(mab_condition, tidyselect::all_of(block_cols))), sep = "_"))
        )
    } else {
      current_data <- current_data |>
        dplyr::mutate(impute_block = mab_condition)
    }


    if (whole_experiment) {
      impute_info <- imputation_information[[1]]
    } else {
      impute_info <- imputation_information[[1]] |>
        dplyr::filter(period_number < i) |>
        dplyr::group_by(treatment_block) |>
        dplyr::summarize(
          success_rate = base::sum(n_success) / base::sum(count),
          failure_rate = 1 - success_rate
        )
    }
    if (!perfect_assignment) {
      dates <- rlang::set_names(
        base::as.Date(base::as.numeric(imputation_information[[2]][i, -1])),
        base::names(imputation_information[[2]][i, -1])
      )
    }

    imputation_info <- check_impute(
      imputation_information = impute_info,
      current_data = current_data
    )


    data <- impute_success(
      current_data = current_data,
      imputation_info = imputation_info,
      dates = dates,
      id_col = {{ id_col }},
      success_col = {{ success_col }},
      prior_data = data,
      perfect_assignment = perfect_assignment,
      success_date_col = {{ success_date_col }}
    )
  }

  bandits <- dplyr::bind_rows(bandits, .id = "period")

  if (algorithm == "UCB1") {
    bandits <- bandits |>
      dplyr::select(ucb, mab_condition, period) |>
      tidyr::pivot_wider(values_from = "ucb", names_from = c("mab_condition"))
  }

  return(list(
    final_data = data,
    bandits = bandits
  ))
}

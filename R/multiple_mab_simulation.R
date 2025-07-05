#' Conducts Multiple Multi-Arm Bandit Trials with Adaptive Inference in Parallel
#'
#' @name multiple_mab_simulation
#' @description Repeated Multi-Arm Bandit Simulations with the same settings in different
#' random states. Allows for parallel processing using [future::plan()] and [furrr::future_map()].
#'
#'
#' @inheritParams single_mab_simulation
#' @param verbose Logical; Toggles progress bar from [furrr::future_map].
#' @param times Numeric; number of simulations to conduct.
#' @param seeds Numeric vector of `length(times)` containing valid seeds to define random state for each trial.
#' @param keep_data Logical; Whether or not to keep the final data from each trial. Recommended FALSE for large datasets
#' .
#' @returns `multiple.mab` class object, which is a named list containing:
#' \item{final_data_nest}{Data.frame containing a nested data.frame with the final data from each trial}
#' \item{bandits}{Data.frame containing the Thompson/UCB1 statitics across all treatments, periods, and trials}
#' \item{estimates}{Data.frame containing the AIPW statitics across all treatments, and trials}
#' \item{settings}{A list of the configuration settings used in the trial.}
#' @seealso
#' * [run_mab_trial()]
#' * [get_adaptive_aipw()]
#' * [check_args()]
#' * [single_mab_simulation()]
#' * [mab_simulation()]
#' * [mab_prepare()]
#' * [furrr::future_map()]
#' * [future::plan]
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
                                    keep_data = FALSE,
                                    control_augment = 0,
                                    time_unit = NULL,
                                    period_length = NULL,
                                    block_cols = NULL,
                                    verbose = FALSE) {
  if (!is.numeric(times) || times < 1) {
    stop("Argument 'times' must be a numeric value greater than or equal to 1.
         Please provide a valid value.")
  }
  if (!is.numeric(seeds) || length(seeds) != times) {
    stop("Argument 'seeds' must be numeric vector of length equal to `times`. Please provide a valid vector.")
  }
  if (!is.logical(keep_data)) {
    stop("Argument 'keep_data' must logical. Please enter `TRUE` or `FALSE`")
  }

  data_name <- deparse(substitute(data))

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


  bandits <- dplyr::bind_rows(
    lapply(mabs[base::seq_len(times)], `[[`, "bandits"),
    .id = "trial"
  ) |>
    dplyr::mutate(trial = base::as.numeric(trial))


  estimates <- dplyr::bind_rows(base::lapply(mabs[base::seq_len(times)], `[[`, "estimates"), .id = "trial") |>
    dplyr::mutate(trial = base::as.numeric(trial))


  if (keep_data) {
    final_data_nest <- tibble::tibble(
      trial = base::seq_len(times),
      data = base::lapply(mabs[seq_len(times)], `[[`, "final_data")
    )
  } else {
    final_data_nest <- NULL
  }


  results <- base::list(
    final_data_nest = final_data_nest,
    bandits = bandits,
    estimates = estimates,
    settings = list(
      data = data_name,
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
  )

  base::class(results) <- c("multiple.mab", class(results))

  return(results)
}

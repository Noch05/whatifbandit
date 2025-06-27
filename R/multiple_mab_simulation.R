#' Conducts Multiple Multi-Arm Bandit Trials with Adapative Inference in Parallel
#'
#' @name multiple_mab_simulation
#' @description
#' This function provides an easy way to repeat the Multi-Arm Bandit Simulations
#' conducted by [single_mab_simulation()] with different random states. Allows for
#' parallel processing using [future::plan()] and [furrr::future_map()].
#'
#'
#' @inheritParams single_mab_simulation
#' @param verbose Logical; Toggles progress bar from [furrr::future_map].
#' @param times Numeric; number of simulations to conduct.
#' @param seeds Numeric vector of `length(times)` containing valid seeds to define random state for each trial.
#' @param keep_data Logical; Whether or not to keep the final data from each trial. Recommended FALSE for large datasets.
#' @returns A custom S3 class multiple.mab object, which is a named list containing:
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
                                    time_unit,
                                    perfect_assignment,
                                    algorithm,
                                    period_length = NULL,
                                    prior_periods,
                                    whole_experiment,
                                    conditions,
                                    blocking,
                                    block_cols = NULL,
                                    date_col,
                                    month_col = NULL,
                                    id_col,
                                    condition_col,
                                    success_col,
                                    success_date_col = NULL,
                                    assignment_date_col = NULL,
                                    verbose,
                                    times, seeds, keep_data) {
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

  check_args(
    data = data, time_unit = time_unit,
    perfect_assignment = perfect_assignment,
    algorithm = algorithm, period_length = period_length,
    whole_experiment = whole_experiment, prior_periods = prior_periods,
    conditions = conditions, blocking = blocking,
    block_cols = block_cols, date_col = {{ date_col }},
    id_col = {{ id_col }}, success_col = {{ success_col }},
    condition_col = {{ condition_col }},
    verbose = verbose,
    success_date_col = {{ success_date_col }},
    assignment_date_col = {{ assignment_date_col }}
  )

  data <- mab_prepare(
    data = data,
    date_col = {{date_col}},
    time_unit = time_unit,
    period_length = period_length,
    success_col = {{success_col}},
    condition_col = {{condition_col}},
    success_date_col = {{success_date_col}},
    month_col = {{month_col}},
    perfect_assignment = perfect_assignment,
    blocking = blocking,
    block_cols = block_cols
  )


  mabs <- furrr::future_map(seq_len(times), ~ {
    set.seed(seeds[.x])
    results <- mab_simulation(
      data = data,
      time_unit = time_unit,
      perfect_assignment = perfect_assignment,
      algorithm = algorithm,
      period_length = period_length,
      prior_periods = prior_periods,
      whole_experiment = whole_experiment,
      conditions = conditions,
      blocking = blocking,
      block_cols = block_cols,
      date_col = {{ date_col }},
      month_col = {{ month_col }},
      id_col = {{ id_col }},
      condition_col = {{ condition_col }},
      success_col = {{ success_col }},
      success_date_col = {{ success_date_col }},
      assignment_date_col = {{ assignment_date_col }},
      verbose = FALSE
    )
    if (!keep_data) {
      results <- results[names(results) != "final_data"]
    }
  },
  .options = furrr::furrr_options(
    globals = list(
      data = data,
      time_unit = time_unit,
      perfect_assignment = perfect_assignment,
      algorithm = algorithm,
      period_length = period_length,
      prior_periods = prior_periods,
      whole_experiment = whole_experiment,
      conditions = conditions,
      blocking = blocking,
      block_cols = block_cols,
      date_col = rlang::enquo(date_col),
      month_col = rlang::enquo(month_col),
      id_col = rlang::enquo(id_col),
      condition_col = rlang::enquo(condition_col),
      success_col = rlang::enquo(success_col),
      success_date_col = rlang::enquo(success_date_col),
      assignment_date_col = rlang::enquo(assignment_date_col),
      times = times,
      seeds = seeds,
      keep_data = keep_data
    ),
    seed = TRUE,
    packages = c(
      "tanfmab", "dplyr", "rlang", "tidyselect",
      "tidyr", "bandit", "tibble", "lubridate",
      "purrr", "furrr", "randomizr"
    )
  ),
  .progress = verbose
  )


  bandits <- dplyr::bind_rows(
    lapply(mabs[base::seq_len(times)], `[[`, "bandits"),
    .id = "trial"
  )
  estimates <- dplyr::bind_rows(base::lapply(mabs[base::seq_len(times)], `[[`, "estimates"), .id = "trial")


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
      data = deparse(substitute(data)),
      time_unit = time_unit,
      perfect_assignment = perfect_assignment,
      algorithm = algorithm,
      period_length = period_length,
      prior_periods = prior_periods,
      whole_experiment = whole_experiment,
      conditions = conditions,
      blocking = blocking,
      block_cols = block_cols,
      trials = times,
      keep_data = keep_data
    )
  )

  base::class(results) <- c("multiple.mab", "mab", "list")

  return(results)
}

#' Conducts Multiple Multi-Arm Bandit Trials with Adaptive Inference in Parallel
#'
#' @name multiple_mab_simulation
#' @description Repeated Multi-Arm Bandit Simulations with the same settings in different
#' random states. Allows for parallel processing using [future::plan()] and [furrr::future_map()].
#'
#'
#' @inheritParams single_mab_simulation
#' @param verbose Logical; Toggles progress bar from [furrr::future_map].
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
#' @seealso
#' * [run_mab_trial()]
#' * [get_adaptive_aipw()]
#' * [check_args()]
#' * [single_mab_simulation()]
#' * [mab_simulation()]
#' * [pre_mab_simulation()]
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
                                    control_augment = 0,
                                    time_unit = NULL,
                                    period_length = NULL,
                                    block_cols = NULL,
                                    verbose = FALSE,
                                    keep_data = FALSE) {
  if ((object.size(data) / (1024^2) > 500)) {
    rlang::warn(c(
      "i" = "`furrr::future_map()` has a serialization limit of 500 MB. If your data
    is larger than that, you have to set the `options(\"future_globals.maxSize\")`
    manually to change it. This has been known to cause failures"
    ))
  }

  if (!is.numeric(times) || times < 1 || floor(times) != times) {
    rlang::abort("Argument 'times' must be an integer value greater than or equal to 1")
  }
  if (!is.integer(seeds) || length(seeds) != times) {
    rlang::abort(c("Argument 'seeds' must be an integer vector of length equal to `times`. Please provide a valid vector.",
      "x" = sprintf("You passed a %s vector.", base::typeof(seeds)),
      "i" = "Reccomended to use `sample.int()` to create proper vector"
    ))
  }
  if (!is.logical(keep_data)) {
    rlang::abort("Argument 'keep_data' must logical. Please enter `TRUE` or `FALSE`")
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
  results <- condense_results(
    data = data, keep_data = keep_data, mabs = mabs,
    times = times
  )

  results$settings <- base::list(
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

  return(results)
}
#' @name condense_results
#' @title Condenses results into a list for [multiple_mab_trial()]
#' @description
#' Takes the output from [furrr:future_map()] in [multiple_mab_trial()]
#' and condenses it to return to the user
#' @inheritParams multiple_mab_simulation
#' @param mabs output from [furrr:future_map()] in [multiple_mab_trial()]
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
        dplyr::bind_rows(.id = "trial") %>%
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

#' Run Multiple Multi-Arm-Bandit Trials with Inference in Parallel
#' @name multiple_mab_simulation
#' @description Performs multiple Multi-Arm Bandit Trials using the same
#' simulation and inference backend as [single_mab_simulation()]. Allows for
#' easy execution of multiple trials under the same settings to gauge the variance
#' of the procedure across execution states. Additionally supports parallel processing
#' through the \href{https://cran.r-project.org/package=future}{future} and
#' \href{https://cran.r-project.org/package=furrr}{furrr} packages.
#'
#' @inheritParams single_mab_simulation
#' @param verbose Logical; Toggles progress bar from [furrr::future_map()] and other intermediate messages.
#' @param times A numeric value of length 1, the number of simulations to conduct.
#' @param seeds An integer vector of `length(times)` containing valid seeds to define random state for each trial.
#' @param keep_data Logical; Whether or not to keep the final data from each trial. Recommended FALSE for large datasets.
#'
#' @returns An object of class `multiple.mab`, containing:
#' \itemize{
#' \item `final_data_nest:` A tibble or data.table containing the nested tibbles/data.tables from each trial. Only provided when `keep_data` is TRUE.
#' \item `bandits`: A tibble or data.table containing the UCB1 valuess or Thompson sampling posterior distributions for each period. Wide format,
#' each row is a period, and each columns is a treatment.
#' \item `assignment_probs`: A tibble or data.table containing the probability of being assigned each treatment arm at a given period. Wide format,
#' each row is a period, and each columns is a treatment.
#' \item `estimates`: A tibble or data.table containing the
#' AIPW (Augmented Inverse Probability Weighting) treatment effect estimates and variances, and traditional
#' sample means and variances, for each treatment arm. Long format, treatment arm, and estimate type are columns along with the mean
#' and variance.
#' \item `settings`: A named list of the configuration settings used in the trial.
#' }
#' @example inst/examples/multiple_mab_simulation_example.R
#' @details
#' Note that when called if data.table has not been attached already it will be when `future.map()` runs,
#' which will print a message to console. This does not mean that if you pass a tibble or data.frame, that data.table will
#' used.
#'
#' ## Implementation
#' This function simulates multiple adaptive Multi-Arm-Bandit Trials, using experimental
#' data from a traditional randomized experiment. It follows the same core procedure as
#' [single_mab_simulation()] (see details, there for a description), but conducts
#' more than one simulation. This allows researchers to gauge the variance
#' of the simulation procedure itself, and use that to form an empirical sampling distribution
#' of the AIPW estimates, instead of relying around asymptotic normality
#' \[\href{https://www.pnas.org/doi/10.1073/pnas.2014602118}{Hadad et al. (2021)}\] for inference.
#'
#' The settings specified here have the same meaning as in [single_mab_simulation()], outside of the additional
#' parameters like `times` and `seeds` which define the number of multiple trials and random seeds to ensure reproducibility.
#' An important note is that `seeds` can only take integer values, so they must be declared or coerced as valid integers,
#' passing doubles (even ones that are mathematical integers) will result in an error. It is recommended to use `sample.int()`,
#' with a known seed beforehand to generate the values. Additionally, it is highly recommended to
#' set `keep_data` to FALSE as the memory used by the function will exponentially increase. This can cause
#' significant performance issues, especially if your system must swap to disk because memory is full.
#'
#' ## Parallel Processing
#'
#' The function provides support for parallel processing via the \href{https://cran.r-project.org/package=future}{future} and
#' \href{https://cran.r-project.org/package=furrr}{furrr} packages. When conducting a large
#' number of simulations, parallelization can improve performance if sufficient system resources are available.
#' Parallel processing must be explicitly set by the user, through `future::plan()`.
#' Windows users should set the plan to "multisession", while Linux and MacOS users can use "multicore" or "multisession".
#' Users running in a High Performance Computing environment (HPC), are encouraged to use
#' \href{https://cran.r-project.org/package=future.batchtools}{future.batchtools},
#' for their respective HPC scheduler.
#' Note that parallel processing is not guaranteed to work on all systems, and may require additional setup or debugging effort
#' from the user. For any issues, users are encouraged to consult the documentation of the above packages.
#' @references
#' Bengtsson, Henrik. 2025. "Future: Unified Parallel and Distributed Processing in R for Everyone."
#' \url{https://cran.r-project.org/package=future}.
#'
#' Bengtsson, Henrik. 2025. "Future.Batchtools: A Future API for Parallel and Distributed Processing Using ‘Batchtools.’"
#' \url{https://cran.r-project.org/package=future.batchtools}.
#'
#' Hadad, Vitor, David A. Hirshberg, Ruohan Zhan, Stefan Wager, and Susan Athey. 2021.
#' "Confidence Intervals for Policy Evaluation in Adaptive Experiments." \emph{Proceedings of the National Academy of Sciences of the United States of America} 118
#' (15): e2014602118. \doi{10.1073/pnas.2014602118}.
#'
#' Kuleshov, Volodymyr, and Doina Precup. 2014. "Algorithms for Multi-Armed Bandit Problems."
#' \emph{arXiv}. \doi{10.48550/arXiv.1402.6028}.
#'
#' Loecher, Thomas Lotze and Markus. 2022. "Bandit: Functions for Simple a/B Split Test and Multi-Armed Bandit Analysis."
#' \url{https://cran.r-project.org/package=bandit}.
#'
#' Offer‐Westort, Molly, Alexander Coppock, and Donald P. Green. 2021.
#' "Adaptive Experimental Design: Prospects and Applications in Political Science."
#' \emph{American Journal of Political Science} 65 (4): 826–44. \doi{10.1111/ajps.12597}..
#'
#' Slivkins, Aleksandrs. 2024. "Introduction to Multi-Armed Bandits." \emph{arXiv}. \doi{10.48550/arXiv.1904.07272}.
#'
#' Vaughan, Davis, Matt Dancho, and RStudio. 2022.
#' "Furrr: Apply Mapping Functions in Parallel Using Futures."
#' \url{https://cran.r-project.org/package=furrr}.
#'
#' @seealso [single_mab_simulation()], \href{https://furrr.futureverse.org}{furrr}, \href{https://future.futureverse.org}{future}
#' @export

multiple_mab_simulation <- function(
  data,
  assignment_method,
  algorithm,
  prior_periods,
  perfect_assignment,
  whole_experiment,
  blocking,
  data_cols,
  times,
  seeds,
  control_augment = 0,
  random_assign_prop = 0,
  ndraws = 5000,
  control_condition = NULL,
  time_unit = NULL,
  period_length = NULL,
  block_cols = NULL,
  verbose = FALSE,
  check_args = TRUE,
  keep_data = FALSE
) {
  if ((utils::object.size(data) / (1024^2) > 500)) {
    rlang::warn(c(
      "i" = "`furrr::future_map()` has a serialization limit of 500 MB. If your data
    is larger than that, you may have to set the `options(\"future_globals.maxSize\")`
    manually to change it."
    ))
  }

  if (!is.numeric(times) || times < 1 || floor(times) != times) {
    rlang::abort(
      c("Argument 'times' must be an integer value greater than or equal to 1"),
      "x" = paste0("You Passed: ", times)
    )
  }
  if (!is.integer(seeds) || length(seeds) != times) {
    rlang::abort(c(
      "Argument 'seeds' must be an integer vector of length equal to `times`. Please provide a valid vector.",
      "x" = sprintf(
        "You passed a %s vector of length %d, while times is %d.",
        base::typeof(seeds),
        base::length(seeds),
        times
      ),
      "i" = "Reccomended to use `sample.int()` to create proper vector"
    ))
  }
  if (!is.logical(keep_data) || is.na(keep_data)) {
    rlang::abort(
      "Argument 'keep_data' must logical. Please enter `TRUE` or `FALSE`"
    )
  }

  prepped <- pre_mab_simulation(
    data = data,
    assignment_method = assignment_method,
    algorithm = algorithm,
    control_condition = control_condition,
    prior_periods = prior_periods,
    perfect_assignment = perfect_assignment,
    whole_experiment = whole_experiment,
    blocking = blocking,
    block_cols = block_cols,
    data_cols = data_cols,
    control_augment = control_augment,
    time_unit = time_unit,
    period_length = period_length,
    verbose = verbose,
    ndraws = ndraws,
    random_assign_prop = random_assign_prop,
    check_args = check_args
  )
  verbose_log(verbose, "Starting Simulations")

  mabs <- furrr::future_map(
    seeds,
    function(x) {
      set.seed(x)
      results <- mab_simulation(
        data = prepped$data,
        time_unit = prepped$character_args$time_unit,
        period_length = period_length,
        prior_periods = prepped$character_args$prior_periods,
        algorithm = prepped$character_args$algorithm,
        whole_experiment = whole_experiment,
        perfect_assignment = perfect_assignment,
        conditions = prepped$conditions,
        blocking = blocking,
        block_cols = prepped$block_cols,
        data_cols = prepped$data_cols,
        verbose = FALSE,
        assignment_method = prepped$character_args$assignment_method,
        control_augment = control_augment,
        imputation_information = prepped$imputation_information,
        ndraws = ndraws,
        random_assign_prop = random_assign_prop
      )
      results$assignment_quantities <- get_assignment_quantities(results)
      if (!keep_data) {
        results$final_data <- NULL
      }
      return(results)
    },
    .options = furrr::furrr_options(
      globals = list(
        mab_simulation = mab_simulation,
        data = prepped$data,
        block_cols = prepped$block_cols,
        data_cols = prepped$data_cols,
        imputation_information = prepped$imputation_information,
        time_unit = prepped$character_args$time_unit,
        period_length = period_length,
        prior_periods = prepped$character_args$prior_periods,
        algorithm = prepped$character_args$algorithm,
        whole_experiment = whole_experiment,
        perfect_assignment = prepped$character_args$perfect_assignment,
        conditions = prepped$conditions,
        blocking = blocking,
        assignment_method = assignment_method,
        control_augment = control_augment,
        keep_data = keep_data,
        ndraws = ndraws,
        random_assign_prop = random_assign_prop
      ),
      packages = c(
        "whatifbandit",
        "dplyr",
        "rlang",
        "tidyr",
        "bandit",
        "tibble",
        "lubridate",
        "purrr",
        "furrr",
        "randomizr",
        "data.table"
      ),
      seed = TRUE
    ),
    .progress = verbose
  )
  verbose_log(verbose, "Collating Results")
  results <- condense_results(
    data = data,
    keep_data = keep_data,
    mabs = mabs,
    times = times
  )

  results$settings <- base::list(
    original_data = data,
    algorithm = prepped$character_args$algorithm,
    assignment_method = prepped$character_args$assignment_method,
    time_unit = prepped$character_args$time_unit,
    period_length = period_length,
    prior_periods = prepped$character_args$prior_periods,
    control_augment = control_augment,
    random_assign_prop = random_assign_prop,
    control = as.character(control_condition),
    conditions = prepped$conditions,
    perfect_assignment = perfect_assignment,
    whole_experiment = whole_experiment,
    blocking = blocking,
    block_cols = prepped$block_cols$name,
    ndraws = ndraws,
    trials = times,
    keep_data = keep_data
  )
  base::class(results) <- c("multiple.mab", class(results))

  return(results)
}
# ------------------------------------------------------------------
#' @name get_assignment_quantitites
#' @title Calculates Number of Observations Assigned to Each Treatment
#' @description Takes the output from [mab_simulation()], and
#' calculates the number of observations assigned to each treatment group in the adaptive trial.
#' @param simulation Output from [mab_simulation()]
#' @returns Named numeric vector containing number of observations assigned to each treatment group
#' @keywords internal
get_assignment_quantities <- function(simulation) {
  UseMethod("get_assignment_quantities", simulation$final_data)
}
#' @method get_assignment_quantities data.frame
#' @description get_assignment_quantities for data.frames
#' @inheritParams get_assignment_quantities
#' @noRd
get_assignment_quantities.data.frame <- function(simulation) {
  count_summary <- simulation$final_data |>
    dplyr::group_by(mab_condition) |>
    dplyr::count()
  count_vec <- rlang::set_names(count_summary$n, count_summary$mab_condition)
  return(count_vec)
}
#-------------------------------------------------------------------
#' @method get_assignment_quantities data.table
#' @description get_assignment_quantities for data.tables
#' @inheritParams get_assignment_quantities
#' @noRd
get_assignment_quantities.data.table <- function(simulation) {
  count_summary <- simulation$final_data[, .N, by = mab_condition]
  data.table::setorder(count_summary, mab_condition)
  count_vec <- rlang::set_names(count_summary$N, count_summary$mab_condition)
  return(count_vec)
}
#-----------------------------------------------------------------

#' @name condense_results
#' @title Condenses results into a list for [multiple_mab_simulation()]
#' @description
#' Takes the output from [furrr::future_map()] in [multiple_mab_simulation()]
#' and condenses it to return to the user.
#' @inheritParams multiple_mab_simulation
#' @param mabs output from [furrr::future_map()] in [multiple_mab_simulation()]
#' @returns `multiple.mab` class object, which is a named list containing:
#' \itemize{
#' \item `final_data_nest:` tibble or data.table containing the nested tibbles/data.tables from each trial. Only provided when `keep_data` is TRUE.
#' \item `bandits`: A tibble or data.table containing the UCB1 valuess or Thompson sampling posterior distributions for each period. Wide format,
#' each row is a period, and each columns is a treatment.
#' \item `assignment_probs`: A tibble or data.table containing the probability of being assigned each treatment arm at a given period. Wide format,
#' each row is a period, and each columns is a treatment.
#' \item `estimates`: A tibble or data.table containing the
#' AIPW (Augmented Inverse Probability Weighting) treatment effect estimates and variances, and traditional
#' sample means and variances, for each treatment arm. Long format, treatment arm, and estimate type are columns along with the mean
#' and variance.
#' \item `settings`: A named list of the configuration settings used in the trial.
#' }
#' @details
#' This function iterates over every element in the output from [furrr::future_map()]
#' and extracts the required element to place to condense into the final list, outputted to the user
#' in [multiple_mab_simulation]. It condenses the long list into tibbles or data.tables, keeping each element
#' together. For example it extracts all the `bandits` objects from the output lists, across all trials, and
#' binds them into a single tibble/data.table.
#'
#' @keywords internal

condense_results <- function(data, keep_data, mabs, times) {
  items <- c(
    "bandits",
    "assignment_probs",
    "estimates",
    "assignment_quantities"
  )

  if (data.table::is.data.table(data)) {
    results <- lapply(items, \(item) {
      all <- lapply(seq_len(times), function(i) {
        if (item == "assignment_quantities") {
          as.list(mabs[[i]][[item]])
        } else {
          mabs[[i]][[item]]
        }
      })
      result <- data.table::rbindlist(all, idcol = "trial", use.names = TRUE)
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

  return(results)
}

#' @title
#' Simulate a Multi-Arm-Bandit Trial from an existing Randomized Controlled Trial, with Bernoulli Distributed Outcomes.
#' @name mab_from_rct.bernoulli
#' @description Performs a single Multi-Arm Bandit (MAB) trial using experimental data from
#' an original randomized controlled trial, and adaptive inference strategies as described in
#' \href{https://www.pnas.org/doi/pdf/10.1073/pnas.2014602118}{Hadad et al. (2021)}.
#' Wraps around the internal implementation functions, and performs the full
#' MAB pipeline: preparing inputs, assigning treatments and imputing successes, and adaptively weighted
#' estimation. See the details and vignettes to learn more.
#'
#' @param formula A `formula` object specifying outcome variable, treatment indicator, treatment blocking and treatment clustering. The treatment variable should always be the first variable following `~`
#' (Additional covariates to be added in later updates). Clustering and blocking variables
#' should being included in specific `block()` or `cluster()` blocks in the formula, e.g. `outcome ~ treatment + block(x1, x2, x3) + cluster(x4)`. Given a set of probabilities for assigning treatments,
#' treatment blocking applies these within each block indpendently instead of the whole sample, while for clustering assignment occurs at the cluster level instead of the individual level, so all observations
#' in a cluster have the same treatment. When blocks and clusters are specified together each cluster must be fully nested in a single block. See below for details. To speciy without clustering or blocking
#' simply do not provide them in the formula. Clusters can only ever be given by 1 variable, while blocks can be a combination of variables.
#'
#' @param data A `data.frame`, `data.table`, or any object which inherits from `data.frame`, containing input data from the trial. This should be the results
#' of a traditional Randomized Controlled Trial (RCT).
#'
#' @param time_unit A character string specifying the unit of time for assigning periods when `assignment_method ="date"`.
#' Acceptable values are `"day", "week",` or `"month"`. `"month"` does not require an additional column with the months of each observation,
#' but it can accept a separate `month_col`. If `month_col` is specified, the periods follow the calendar months strictly, and when it is not
#' specified months are simply used as the time interval. For example if a dataset has dates starting on July 26th, under month based assignment and
#' a specified `month_col` the dates July 26th and August 3st would be in different periods, but if the `month_col` was not specified, they would be
#' in the same period because the dates are less than one month apart.
#'
#' @param algorithm A character string specifying the MAB algorithm to use. Options are `"Thompson"` or `"UCB1"`, ignoring case. Algorithm
#' defines the adaptive assignment process. For more details on these specific algorithms see
#' \href{https://www.jstor.org/stable/2332286}{Thompson 1933};\href{https://doi.org/10.1023/A:1013689704352Auer et al. 2002};
#' \href{https://proceedings.mlr.press/v23/agrawal12.html}{Agrawal and Goyal 2012};\href{https://arxiv.org/abs/1402.6028}{Kuleshov and Precup 2014} and
#' \href{https://arxiv.org/abs/1904.07272}{Slivkins 2024}.
#'
#' @param period_method A character string; one of `"date"`, `"batch"`, or `"individual"`, to define the assignment into treatment waves. When using
#' `"batch"` or `"individual"`, ensure your dataset is pre-arranged in the proper order observations should be considered so that
#' groups are assigned correctly. For "date", observations will be considered in chronological order.
#' `"individual"` assignment can be computationally intensive for larger datasets.
#'
#' @param period_length A positive integer; represents the length of each treatment period.
#' If `assignment_method` is "date", this length refers the number of units specified in `time_unit`.
#' (i.e., if `"day"``, 10 would be 10 days). If `assignment_method` = `"batch"`, this refers to the number of people in each batch.
#'
#' @param prior_periods A positive integer; number of previous periods to use
#' in the treatment assignment model. Default is `NULL`, where all prior periods are considered. See below for details.
#'
#' @param whole_experiment Logical; if `TRUE`, uses all past experimental data for imputing outcomes.
#' If `FALSE`, uses only data available up to the current period. In large datasets or with a high number
#' of periods, setting this to `FALSE` can be more computationally intensive, though not a significant
#' contributor to total run time. Default is `FALSE`.
#'
#' @param delayed_feedback Logical; if `FALSE`, assumes instanteneous feedback for outcomes, as soon as a treatment is assigned, the outcome is realized and known.
#' If `TRUE`, delayed feedback is assumed, so  as soon as treatment is assigned, a potential outcome is realized, but it is not known to the simulation, until the specified date.
#' When re-computing the adaptive assignment probabilities via Thompsom Samplings or UCB1 outcomes that have not been observed on the date the assignments are made will be treated as
#' failures. See details below for more information.
#'
#' @param id_col Column in `data`; contains unique ID as a key.
#' @param date_col Column in `data`; contains original date of event/trial. Only necessary when assigning by "Date". Must be of type `Date`, not a character string.
#' @param month_col Column in `data`; contains month of treatment. Only necessary when `time_unit = "Month"`, and when periods should be determined directly by
#' the calendar months instead of month based time periods. This column can be a string/factor variable with the month names or numeric with the month number. It can easily
#' be created from your `date_col` via `lubridate::month(data[[date_col]])` or `format(data[[date_col]], "%m")`.
#' @param success_date_col Column in `data`; contains original dates each success occurred. Only necessary when `perfect_assignment = FALSE`. Must be of type `Date`, not a character string.
#' @param assignment_date_col Column in `data`; contains original dates treatments were assigned to observations. Only necessary when `perfect_assignment = FALSE`.
#' Used to simulate imperfect information on the part of researchers conducting an adaptive trial. Must be of type `Date`, not a character string.
#'
#' @param control_condition Value of the control condition. Only necessary when `control_augment` is greater than 0. Internally this value
#' is coerced to a string, so it should be passed as a string, or a type that can easily be converted to a string.
#'
#' @param control_augment A number ranging from 0 to 1; proportion of each wave guaranteed to receive the "Control" treatment.
#' Default is 0. It is not recommended to use this in conjunction with `random_assign_prop`.
#'
#' @param verbose Logical; whether or not to print intermediate messages. Default is `FALSE`.
#'
#' @param ndraws A positive integer; When Thompson sampling direct calculations fail, draws from a simulated posterior
#' will be used to approximate the Thompson sampling probabilities. This is the number of simulations to use, the default
#' is 5000 to match the default parameter [bandit::best_binomial_bandit_sim()], but might need to be raised or lowered depending on performance and accuracy
#' concerns.
#'
#' @param random_assign_prop Proportion of batch to be assigned new treatments via static, equal probabilities instead of the
#' adpatively updated probabailities. Ranges from 0 to 1. For example if this is set to `0.1` then 90% of that batch will be assigned
#' using the set of probabilities created by Thomspon Sampling or UCB1, while 10% is assigned using equal probabilities for each treatment.
#' It is not recommended to use this in conjunction with `control_augment`. Under small batch sizes, where `random_assign_prop * size < 1`, this value instead represents
#' the probability that the given unit will be assigned under uniform probabilities, as opposed to the adaptive ones. Otherwise, it represents the proportion of rows,
#' rounded to a whole number, which will be selected.
#'
#' @param impute_cluster Logical; If `TRUE` imputation procedure uses the cluster means from the original trial as the probabilities of success.
#' Set to `FALSE` when clusters are extremely small so using these values for imputations is not desired. Here, imputation falls black to the estimates within each treatment and block combination,
#' or just simply within each treatment depending on if blocks were provided.
#'
#' @param r Positive integer; number of replications (under different random seed). Replications of the MAB procedure on a fixed dataset provides important diagnostic information on the stochasticity/variance of
#' the re-simulation method. Replications can be conducted in parallel, by setting an appropriate [future::plan()].
#'
#' @param seeds An integer vector of `length(r)` containing valid seeds to define random state for each re-simulation.
#' @param keep_data Logical; Whether or not to keep the final data from each trial. Recommended `FALSE`. When `r = 1` the final data is always kept and reported.
#' @param check_args Logical; Whether or not to robustly check whether arguments are valid. Default is TRUE, and recommended
#' not to be changed.
#'
#' @returns An object of class `mab`, containing:
#' \itemize{
#' \item `final_data`: The processed `tibble` or `data.table`, containing new columns pertaining to the results of the trial. Specifically Contains:
#' \itemize{
#' \item `period_number`: Assigned period for simulation.
#' \item `mab_*`: New treatment conditions and outcomes under the simulation.
#' \item `impute_req`: Whether observation required an imputed outcome.
#' \item `*block`: variables relating to the block specified for treatment blocking, and the concatenation
#' of that block with an observations original treatment, and new treatment.
#' \item `aipw_*` Columns containing individual Augmented Inverse Probability Weighted estimates for each observation and treatment arm.
#' \item `prior_rate_*`: Columns containing success rate for each treatment arm, from all periods before the observations period of the simulation.
#' \item `*_assign_prob`: Columns containing probability of being assigned each treatment at the given period.
#' }
#' \item `bandits`: A `tibble` or `data.table` containing the UCB1 values or Thompson sampling posterior distributions for each period. Wide format,
#' each row is a period, and each columns is a treatment. Each row in this table represents the calculation from the given period
#' after its values were imputed, so row 2 represents the calculations made in period 3, but represent the impact of period 2's new assignments.
#' \item `assignment_probs`: A `tibble` or `data.table` containing the probability of being assigned each treatment arm at a given period. Wide format,
#' each row is a period, and each columns is a treatment. Each row represents the probability of being assigned each treatment at each period, these have not
#' been shifted like the bandits table.
#' \item `estimates`: A `tibble` or `data.table` containing the
#' AIPW (Augmented Inverse Probability Weighting) treatment effect estimates and variances, and traditional
#' sample means and variances, for each treatment arm. Long format, treatment arm, and estimate type are columns along with the mean
#' and variance.
#' \item `settings`: A named list of the configuration settings used in the trial.
#' }
#'
#' @details
#' For all the items laballed as a `data.frame` or `data.table`, `data.table`s will be used if the user passed `data` is a
#' `data.table`, `data.frame`s used otherwise.
#'
#' ## Clustering
#' Under adaptive probabilities of assignment,
#' traditional cluster assignment no longer makes sense, as then the updated probabilities have no impact.
#' Thus clustering is only performed at the period level, so across the experiment observations
#' in the same cluster may get different assignments.
#' In this case cluster level treatment effect estimates are not valid, so the user should ensure that clusters only persist across 1 period.
#'
#' ## Prior Periods
#' It makes sense to consider using less than all of the available information to make treatment assignments, when you consider
#' a non-stationary bandit problem. If you believe the true probabilities of success for a treatment can change over time you may only want
#' to consider a limited number of the prior periods as older information is "less" useful in determining what treatment assignments should be.
#'
#' ## Implementation
#'
#' At each period, either the Thompson sampling probabilities or UCB1 values are calculated based on
#' the outcomes from the number of `prior_periods` specified. New treatments are then assigned randomly using the Thompson
#' sampling probabilities via the \href{https://cran.r-project.org/package=randomizr}{randomizr}
#' package, or as the treatment with the highest UCB1 values, while implementing the specific
#' treatment blocking and control augmentation specified.
#'
#' If a hybrid assignment is specified, here is where it is implemented in the simulation.
#' `control_augment` is a threshold probability for the control group, and the assignment probabilities
#' are changed to ensure that threshold is met. The other hybrid assignment is `random_assign_prop`. Here, the specified
#' proportion of the data is set aside to assign treatments randomly, while the rest of the data is assigned through the bandit procedure.
#'
#' After assigning treatments, observations with new treatments have their outcomes imputed
#' using success rates from the original randomized trial. These rates are estimated as grouped
#' means within each treatment arm. If blocking is specified, rates are estimated within each
#' combination of treatment arm and block. If clustering is specified, rates are estimated
#' within each cluster, since all observations in a cluster receive the same treatment.
#'
#' If `perfect_assignment = FALSE`, new dates of success will be imputed using the means
#' of those dates in the period, grouped by treatment block/cluster if necessar. Observations for which
#' their treatment changed, but their outcome was success in the original and simulation, do not have their date changed.
#' When the next period starts, the success dates are checked against the maximum/latest `assignment_date` for the period, and
#' if any success occurs after that, it is treated as a failure for the purpose of the bandit decision algorithms.
#'
#' At the end of the simulation the results are aggregated together to calculate the Adaptively Weighted
#' Augmented Inverse Probability Estimator (Hadad et al. 2021) using the mean and variance formulas provided, under
#' the constant allocation rate adaptive schema. These estimators are unbiased and asymptotically normal under the adaptive
#' conditions which is why they are used. For a complete view of their properties, reading the paper is recommended.
#'
#' ## Performance Concerns
#'
#' This procedure has the potential to be computationally expensive and time-consuming. Performance
#' depends on the relative size of each period, number of periods, and overall size of the dataset. This function has
#' separate support for `data.frame`s and `data.table`s. If a `data.frame` is passed, the function uses a combination of `dplyr`, `tidyr`
#' and base `R` to shape data, and run the simulation. However, if a `data.table` is passed the function exclusively uses the `data.table`
#' code for all the same operations.
#'
#' In general, smaller batches run faster under base `R`, while larger ones could benefit from the performance
#' and memory efficiencies provided by `data.table`. However, we've observed larger datasets can cause numerical
#' instability with some calculations in the Thompson sampling procedure. Internal safeguards exist to prevent this, but
#' the best way to preempt any issues is to set `prior_periods` to a low number.
#'
#' ## `r > 1`
#' Multiple simulations allows researchers to gauge the variance
#' of the simulation procedure itself, by repeating it several times under different random states, using the same fixed data
#'
#' ### Parallel Processing
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
#'
#' For more information about how to use the function, please view the vignette.
#'
#' @references
#'
#' Agrawal, Shipra, and Navin Goyal. 2012.
#' "Analysis of Thompson Sampling for the Multi-Armed Bandit Problem."
#' \emph{Proceedings of the 25th Annual Conference on Learning Theory}, June 16, 39.1-39.26.
#' \url{https://proceedings.mlr.press/v23/agrawal12.html}.
#'
#' Auer, Peter, Nicolò Cesa-Bianchi, and Paul Fischer. 2002.
#' "Finite-Time Analysis of the Multiarmed Bandit Problem." \emph{Machine Learning}
#' 47 (2): 235–56. \doi{10.1023/A:1013689704352}.
#'
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
#' @seealso \href{https://furrr.futureverse.org}{furrr}, \href{https://future.futureverse.org}{future}, [summary.mab()], [plot.mab()].
#'
#' @example inst/examples/single_mab_simulation_example.R
#' @export
mab_from_rct.bernoulli <- function(
  formula,
  data,
  algorithm,
  period_method,
  period_length,
  prior_periods,
  id_col,
  date_col = NULL,
  month_col = NULL,
  assignment_date_col = NULL,
  success_date_col = NULL,
  r = 1,
  control_augment = 0,
  random_assign_prop = 0,
  ndraws = 5000,
  impute_cluster = FALSE,
  delayed_feedback = FALSE,
  whole_experiment = FALSE,
  control_condition = NULL,
  time_unit = NULL,
  seeds = NULL,
  verbose = FALSE,
  check_args = TRUE,
  keep_data = FALSE
) {
  data_cols <- c(
    formula_parse(formula),
    id_col = id_col,
    date_col = date_col,
    month_col = month_col,
    assignment_date_col = assignment_date_col,
    success_date_col = success_date_col
  )
  blocking <- !base::is.null(data_cols$block_cols)
  clustering <- !base::is.null(data$cluster_col)

  prepped <- prep_rct_data(
    data = data,
    period_method = period_method,
    algorithm = algorithm,
    control_condition = control_condition,
    prior_periods = prior_periods,
    delayed_feedback = delayed_feedback,
    whole_experiment = whole_experiment,
    blocking = blocking,
    clustering = clustering,
    data_cols = data_cols,
    control_augment = control_augment,
    time_unit = time_unit,
    period_length = period_length,
    check_args = check_args,
    verbose = verbose,
    ndraws = ndraws,
    random_assign_prop = random_assign_prop,
    r = r,
    seeds = seeds,
    keep_data = keep_data,
    impute_cluster = impute_cluster
  )

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
    verbose = verbose,
    assignment_method = prepped$character_args$assignment_method,
    control_augment = control_augment,
    imputation_information = prepped$imputation_information,
    ndraws = ndraws,
    random_assign_prop = random_assign_prop
  )
  results$settings <- list(
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
    ndraws = ndraws
  )
  class(results) <- c("mab", class(results))

  return(results)
}
#------------------------------------------------------------------------------
#' Formula Parser
#' @description
#' Parsers the input formula for [mab_from_rct.bernoulli()]
#' @name formula_parse
#' @inheritParams mab_from_rct.bernoulli
#' @returns
#' @keywords internal

formula_parse <- function(formula) {
  formula <- as.character(formula)

  outcome <- formula[2]

  obc <- base::strsplit(formula[3], "\\+") |>
    base::lapply(base::trimws) |>
    base::unlist()

  conditions_col <- obc[1]
  other_vars <- base::lapply(
    base::list(
      obc[base::grepl("block\\((.*?)\\)", obc)],
      obc[base::grepl("cluster\\((.*?)\\)", obc)]
    ),
    gather_args
  )

  return(
    base::list(
      condition_col = conditions_col,
      success_col = outcome,
      block_cols = block(other_vars[[1]]$args),
      cluster_col = cluster(other_vars[[2]]$args)
    )
  )
}

gather_args <- function(x) {
  if (base::length(x) == 0) {
    return(base::list(NULL))
  }
  call <- rlang::parse_expr(x) |>
    base::as.list()

  args <- base::vapply(
    call[-1],
    rlang::as_label,
    base::character(1)
  )
  return(list(call = call[[1]], args = args))
}

block <- function(...) {
  base::c(...)
}
cluster <- function(x) {
  x
}


#'
#' Verbose Printer
#' @description Shorthand Function for checking `verbose` and then printing if TRUE
#' @name verbose_log
#' @param message The message to be printed to screen, as a string.
#' @param log Logical; Whether or not to print the message, this will always be
#' the `verbose` argument passed from higher functions.
#' @returns Text output of `message` to the console when `log = TRUE`. If
#' `log = FALSE`, returns nothing.
#' @keywords internal

verbose_log <- function(log, message) {
  if (log) {
    base::cat(message, "\n")
  }
}
#' @name get_assignment_quantitites
#' @title Calculates Number of Observations Assigned to Each Treatment
#' @description Takes the output from [mab_simulation()], and
#' calculates the number of observations assigned to each treatment group in the adaptive trial.
#' @param simulation Output from [mab_simulation()]
#' @param conditions Character vector containing the names of all the treatment conditions in the trial.
#' @returns Named numeric vector containing number of observations assigned to each treatment group
#' @keywords internal
get_assignment_quantities <- function(simulation, conditions) {
  UseMethod("get_assignment_quantities", simulation$final_data)
}
#' @method get_assignment_quantities data.frame
#' @description get_assignment_quantities for data.frames
#' @inheritParams get_assignment_quantities
#' @noRd
get_assignment_quantities.data.frame <- function(simulation, conditions) {
  count_summary <- simulation$final_data |>
    dplyr::group_by(mab_condition) |>
    dplyr::count()

  count_vec <- rlang::set_names(count_summary$n, count_summary$mab_condition)

  if (length(count_vec) < length(conditions)) {
    missing_conds <- base::setdiff(
      conditions,
      base::names(count_vec)
    )
    count_vec[missing_conds] <- 0
  }
  return(count_vec)
}
#-------------------------------------------------------------------
#' @method get_assignment_quantities `data.table`
#' @description get_assignment_quantities for `data.table`s
#' @inheritParams get_assignment_quantities
#' @noRd
get_assignment_quantities.data.table <- function(simulation, conditions) {
  count_summary <- simulation$final_data[, .N, by = mab_condition]
  data.table::setorder(count_summary, mab_condition)
  count_vec <- rlang::set_names(count_summary$N, count_summary$mab_condition)
  if (length(count_vec) < length(conditions)) {
    missing_conds <- base::setdiff(
      conditions,
      base::names(count_vec)
    )
    count_vec[missing_conds] <- 0
  }
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
#' \item `final_data_nest:` `tibble` or `data.table` containing the nested `tibble`s/`data.table`s from each trial. Only provided when `keep_data = TRUE`.
#' \item `bandits`: A `tibble` or `data.table` containing the UCB1 values or Thompson sampling posterior distributions for each period. Wide format,
#' each row is a period, and each columns is a treatment.
#' \item `assignment_probs`: A `tibble` or `data.table` containing the probability of being assigned each treatment arm at a given period. Wide format,
#' each row is a period, and each columns is a treatment.
#' \item `estimates`: A `tibble` or `data.table` containing the
#' AIPW (Augmented Inverse Probability Weighting) treatment effect estimates and variances, and traditional
#' sample means and variances, for each treatment arm. Long format, treatment arm, and estimate type are columns along with the mean
#' and variance.
#' \item `settings`: A named list of the configuration settings used in the trial.
#' }
#' @details
#' This function iterates over every element in the output from [furrr::future_map()]
#' and extracts the required element to place to condense into the final list, outputted to the user
#' in [multiple_mab_simulation]. It condenses the long list into `tibble`s or `data.table`s, keeping each element
#' together. For example it extracts all the `bandits` objects from the output lists, across all trials, and
#' binds them into a single `tibble`/`data.table`.
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

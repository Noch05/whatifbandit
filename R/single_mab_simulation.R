#' @title
#' Run One Adaptive Simulation With Inference.
#' @name single_mab_simulation
#'
#' @description Performs a single Multi-Arm Bandit (MAB) trial using experimental data from
#' an original randomized controlled trial, and adaptive inference strategies as described in
#' \href{https://www.pnas.org/doi/pdf/10.1073/pnas.2014602118}{Hadad et al. (2021)}.
#' Wraps around the internal implementation functions, and performs the full
#' MAB pipeline: preparing inputs, assigning treatments and imputing successes, and adaptively weighted
#' estimation. See the details and vignettes to learn more.
#'
#' @param data A data.frame, data.table, or tibble containing input data from the trial. This should be the results
#' of a traditional Randomized Controlled Trial (RCT). data.frames will be converted to tibbles internally.
#'
#' @param time_unit A character string specifying the unit of time for assigning periods when `assignment_method` is "Date".
#' Acceptable values are "Day", "Week", or "Month". "Month" is a special case that is useful when an experiment
#' defines the months differently then the genuine dates, i.e. an experiment considers August as starting
#' the in the second half of July, or when exact calender months are required for the periods, not just
#' a lengths of time in the month range. As such it requires an additional column to be provided,
#' with the exact month desired for each observation passed as an integer, character, or factor. Each
#' observation is treated as occurring on the first of each month.
#'
#' @param perfect_assignment A logical value; if TRUE, assumes perfect information for treatment assignment
#' (i.e., all outcomes are observed regardless of the date).
#' If FALSE, hides outcomes not yet theoretically observed, based
#' on the dates treatments would have been assigned for each wave.
#' This is useful when simulating batch-based assignment where treatments were assigned
#' on a given day whether or not all the information from a prior batch was available and
#' you have exact dates treatments were assigned.
#'
#' @param algorithm A character string specifying the MAB algorithm to use. Options are "Thompson" or "UCB1". Algorithm
#' defines the adaptive assignment process. Mathematical details on these algorithms
#' can be found in \href{https://arxiv.org/abs/1402.6028}{Kuleshov and Precup 2014} and
#' \href{https://arxiv.org/abs/1904.07272}{Slivkins 2024}.
#'
#' @param period_length A numeric value of length 1; represents the length of each treatment period.
#' If assignment method is "Date", this length refers the number of units specified in`time_unit`
#' (i.e., if "Day", 10 would be 10 days).
#' If assignment method is "Batch", this refers to the number of people in each batch. This factor
#' contributes most to the computational cost of calling the function, as large batch sizes make each iteration of
#' the simulation run slower, while each additional period adds time because of the extra iterations, so be exercise
#' caution with this argument.
#'
#' @param prior_periods A numeric value of length 1, or the character string "All"; number of previous periods to use
#' in the treatment assignment model. This is used to implement the stationary/non-stationary bandit.
#' For example, a non-stationary bandit assumes the true probability of success for each treatment changes over time, so to
#' account for that, not all prior data should be used when making decisions because it could be "out of date".
#'
#' @param whole_experiment A logical value; if TRUE, uses all past experimental data for imputing outcomes.
#' If FALSE, uses only data available up to the current period. In large datasets or with a high number
#' of periods, setting this to FALSE can be more computationally intensive, though not a significant
#' contributor to total run time.
#'
#' @param conditions A named character vector containing treatment conditions. The elements
#' of this vector should be the names of each treatment as seen in your data, so to create it you can simply call
#' `unique(df[[condition_col]])`. The names of each element are used to reference the contents but are not inherently important;
#' choose names that are meaningful and consistent. If `control_augment` > 0, then the control condition
#' of the trial in this vector must have the name "Control".
#'
#' @param data_cols A named character vector containing the names of columns in `data` as strings:
#' \itemize{
#' \item `id_col`: Column in `data`; contains unique ID as a key.
#' \item `success_col`: Column in `data`; binary successes from the original experiment.
#' \item `condition_col`: Column in `data`; original treatment condition for each observation.
#' \item `date_col`: Column in `data`; contains original date of event/trial. Only necessary when assigning by "Date". Must be of type `Date`, not a character string.
#' \item `month_col`: Column in `data`; contains month of treatment. Only necessary when `time_unit = "Month"`. This can be a string or factor variable
#' containing the names or numbers of months.
#' \item `success_date_col`: Column in `data`; contains original dates each success occurred. Only necessary when `perfect_assignment = FALSE`. Must be of type `Date`, not a character string.
#' \item `assignment_date_col`: Column in `data`; contains original dates treatments were assigned to observations. Only necessary when `perfect_assignment = FALSE`.
#' Used to simulate imperfect information on the part of researchers conducting an adaptive trial. Must be of type `Date`, not a character string.
#' }
#'
#' @param blocking A logical value; whether or not to use treatment blocking. Treatment blocking is used to ensure an even-enough
#' distribution of treatment conditions across blocks. For example, blocking by gender would mean the randomized assignment should
#' split treatments evenly not just throughout the sample (so for 4 arms, 25-25-25-25), but also within each block, so 25% of men
#' would receive each treatment and 25% of women the same. This is most useful when the blocks are uneven or have some geographic meaning.
#'
#' @param block_cols A character vector of variables to block by. This vector should not be named.
#'
#' @param assignment_method A character string; one of "Date", "Batch", or "Individual", to define the assignment into treatment waves. When using
#' "Batch" or "Individual", ensure your dataset is pre-arranged in the proper order observations should be considered so that
#' groups are assigned correctly. For "Date", observations will be considered in chronological order.
#' "Individual" assignment can be time-consuming for larger datasets.
#'
#' @param control_augment A numeric value ranging from 0 to 1; proportion of each wave guaranteed to receive the "Control" treatment.
#' Default is 0. It is not recommended to use this in conjunction with `random_assign_prop`.
#'
#' @param verbose A logical value; whether or not to print intermediate messages. Default is FALSE.
#'
#' @param ndraws A numeric value; When Thompson Sampling direct calculations fail, draws from a simulated posterior
#' will be used to approximate the Thompson Sampling probabilities. This is the number of simulations to use, the default
#' is 5000 to match the default parameter [bandit::best_binomial_bandit_sim()], but might need to be raised or lowered depending on performance and accuracy
#' concerns.
#'
#' @param random_assign_prop a numeric value ranging from 0 to 1; proportion of each wave to be assigned new treatments randomly,
#' 1 - `random_assign_prop` is the proportion assigned through the bandit procedure. For example if this is set to 0.1, then
#' for each wave 10% of the observations will be randomly assigned to a new treatment, while the remaining 90% will be assigned according
#' to UCB1 or Thompson result. It is not recommended to use this in conjunction with `control_augment`. If batch sizes are small,
#' and the number of rows is calculate to be less than 1, and probability sampling approach is used where each row in the batch
#' will have a `random_assign_prop` probability of being selected for random assignment. Otherwise the number is rounded to
#' a whole number, and that many rows are selected for random assignment.
#'
#' @param check_args Logical; Whether or not to robustly check whether arguments are valid. Default is TRUE, and recommended
#' not to be changed.
#'
#' @returns An object of class `mab`, containing:
#' \itemize{
#' \item `final_data`: The processed tibble or data.table, containing new columns pertaining to the results of the trial. Specifically Contains:
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
#' \item `bandits`: A tibble or data.table containing the UCB1 statistics or Thompson Sampling posterior distributions for each period. Wide format,
#' each row is a period, and each columns is a treatment.
#' \item `assignment_probs`: A tibble or data.table containing the probability of being assigned each treatment arm at a given period. Wide format,
#' each row is a period, and each columns is a treatment.
#' \item `estimates`: A tibble or data.table containing the
#' AIPW (Augmented Inverse Probability Weighting) treatment effect estimates and variances, and traditional
#' sample means and variances, for each treatment arm. Long format, treatment arm, and estimate type are columns along with the mean
#' and variance.
#' \item `settings`: A named list of the configuration settings used in the trial.
#' }
#'
#' @details
#' This function simulates a single adaptive Multi-Arm-Bandit trial, using experimental data from
#' a traditional randomized controlled trial. It is intended to help researchers understand how an adaptive design could have performed
#' but it is not a substitute for a real experiment, and for that reason it does not generate the synthetic data for the simulation.
#' The input data should come from a randomized trial to ensure the assumptions made during the simulation are valid.
#'
#' At each period, either the Thompson Probabilities or UCB1 statistics are calculated based on
#' the outcomes from the number of `prior_periods` specified. New treatments are then assigned randomly using the Thompson
#' Probabilities via the \href{https://cran.r-project.org/package=randomizr}{randomizr}
#' package, or as the treatment with the highest UCB1 statistic, while implementing the specific
#' treatment blocking and control augmentation specified. More details on bandit algorithms can in
#' \href{https://arxiv.org/abs/1402.6028}{Kuleshov and Precup 2014} and
#' \href{https://arxiv.org/abs/1904.07272}{Slivkins 2024}.
#'
#' If a hybrid assignment is specified, here is where it is implemented in the simulation.
#' `control_augment` is a threshold probability for the control group, and the assignment probabilities
#' are changed to ensure that threshold is met. The other hybrid assignment is `random_assign_prop`. Here, the specified
#' proportion of the data is set aside to assign treatments randomly, while the rest of the data is assigned through the bandit procedure.
#'
#' After assigning treatments, observations with new treatments have their outcomes imputed, with any
#' specified treatment blocking. The probabilities of success used to impute,
#' are estimated via the grouped means of successes from the original data either from the whole trial, or
#' up to that period, defined by `whole_experiment`.
#'
#' If `perfect_assignment` is FALSE, new dates of success will be imputed using averages
#' of those dates in the period, grouped by treatment block. Observations for which
#' their treatment changed, but their outcome was success in the original and simulation, do not have their date changed.
#' When the next period starts, the success dates are checked against the maximum/latest `assignment_date` for the period, and
#' if any success occurs after that, it is treated as a failure for the purpose of the bandit decision algorithms.
#'
#' At the end of the simulation the results are aggregated together to calculate the Adaptively Weighted
#' Augmented Inverse Probability Estimator (Hadad et al. 2021) using the mean and variance formulas provided, under
#' the constant allocation rate adaptive schema. These estimators are unbiased and asymptotically normal under the adaptive
#' conditions which is why they are used. For a complete view of their properties, reading the paper is recommended.
#'
#' This procedure has the potential to be computationally expensive and time-consuming. Performance
#' depends on the relative size of each period, number of periods, and overall size of the dataset. This function has
#' separate support for data.frames and data.tables. If a data.frame is passed, the function uses a combination of dplyr, tidyr
#' and base R to shape data, and run the simulation. However, if a data.table is passed the function exclusively uses the data.table
#' code for all the same operations.
#'
#' In general, smaller batches run faster under base R, while larger ones could benefit from the performance
#' and memory efficiencies provided by data.table. However, we've observed larger datasets can cause numerical
#' instability with some calculations in the Thompson Sampling procedure. Internal safeguards exist to prevent this, but
#' the best way to preempt any issues is to set `prior_periods` to a low number.
#'
#' For more information about how to use the function, please view the vignette.
#'
#' @seealso [multiple_mab_simulation()], [summary.mab()], [plot.mab()].
#' @references
#'
#' Hadad, Vitor, David A. Hirshberg, Ruohan Zhan, Stefan Wager, and Susan Athey. 2021.
#' “Confidence Intervals for Policy Evaluation in Adaptive Experiments.” \emph{Proceedings of the National Academy of Sciences of the United States of America} 118
#' (15): e2014602118. \doi{10.1073/pnas.2014602118}.
#'
#' Kuleshov, Volodymyr, and Doina Precup. 2014. “Algorithms for Multi-Armed Bandit Problems.”
#' \emph{arXiv}. \doi{10.48550/arXiv.1402.6028}.
#'
#' Loecher, Thomas Lotze and Markus. 2022.
#' “Bandit: Functions for Simple a/B Split Test and Multi-Armed Bandit Analysis.”
#' \url{https://cran.r-project.org/package=bandit}.
#'
#' Offer‐Westort, Molly, Alexander Coppock, and Donald P. Green. 2021.
#' “Adaptive Experimental Design: Prospects and Applications in Political Science.”
#' \emph{American Journal of Political Science} 65 (4): 826–44. \doi{10.1111/ajps.12597}.
#'
#' Slivkins, Aleksandrs. 2024. “Introduction to Multi-Armed Bandits.” \emph{arXiv}. \doi{10.48550/arXiv.1904.07272}.
#' @example inst/examples/single_mab_simulation_example.R
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
                                  random_assign_prop = 0,
                                  ndraws = 5000,
                                  time_unit = NULL,
                                  period_length = NULL,
                                  block_cols = NULL,
                                  verbose = FALSE,
                                  check_args = TRUE) {
  prepped <- pre_mab_simulation(
    data = data, assignment_method = assignment_method,
    algorithm = algorithm, conditions = conditions,
    prior_periods = prior_periods, perfect_assignment = perfect_assignment,
    whole_experiment = whole_experiment, blocking = blocking,
    block_cols = block_cols, data_cols = data_cols,
    control_augment = control_augment, time_unit = time_unit,
    period_length = period_length, check_args = check_args,
    verbose = verbose, ndraws = ndraws, random_assign_prop = random_assign_prop
  )
  ## Initial Sort for Consistency in calling by numeric indexes
  conditions <- base::sort(conditions)


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
    imputation_information = prepped$imputation_information,
    ndraws = ndraws,
    random_assign_prop = random_assign_prop
  )
  results$settings <- list(
    original_data = data,
    assignment_method = assignment_method,
    control_augment = control_augment,
    random_assign_prop = random_assign_prop,
    time_unit = time_unit,
    perfect_assignment = perfect_assignment,
    algorithm = algorithm,
    period_length = period_length,
    prior_periods = prior_periods,
    whole_experiment = whole_experiment,
    conditions = conditions,
    blocking = blocking,
    block_cols = prepped$block_cols$name,
    ndraws = ndraws
  )
  class(results) <- c("mab", class(results))

  return(results)
}
#------------------------------------------------------------------------------
#' @name cols
#' @title Column arguments shared across functions
#' @description Topic holding common arguments across many functions,
#' used to inherit from in documentation to keep definitions consistent. Not a
#' function.
#' @param id_col Column in `data`; contains unique ID as a key.
#' @param success_col Column in `data`; binary successes from the original experiment.
#' @param condition_col Column in `data`; original treatment condition for each observation.
#' @param date_col Column in `data`; contains original date of event/trial. Only necessary when assigning by "Date". Must be of type `Date`, not a character string.
#' @param month_col Column in `data`; contains month of treatment. Only necessary when `time_unit = "Month"`. This can be a string or factor variable
#' containing the names or numbers of months.
#' @param success_date_col Column in `data`; contains original dates each success occurred. Only necessary when `perfect_assignment = FALSE`. Must be of type `Date`, not a character string.
#' @param assignment_date_col Column in `data`; contains original dates treatments were assigned to observations. Only necessary when `perfect_assignment = FALSE`.
#' Used to simulate imperfect information on the part of researchers conducting an adaptive trial. Must be of type `Date`, not a character string.
#' @keywords internal
NULL

#' Verbose Printer
#' @description Shorthand Function for checking `verbose` and then printing if TRUE
#' @name verbose_log
#' @param message The message to be printed to screen, as a string.
#' @param log Logical; Whether or not to print the message, this will always be
#' the `verbose` argument passed from higher functions.
#' @returns Text output of `message` to the console when `log` is TRUE. If
#' `log` is FALSE, returns nothing.
#' @keywords internal

verbose_log <- function(log, message) {
  if (log) {
    base::cat(message, "\n")
  }
}

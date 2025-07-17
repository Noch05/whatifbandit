#' @title
#' Run One Adaptive Simulation With Inference.
#' @name single_mab_simulation
#'
#' @description Performs a single Multi-Arm Bandit (MAB) trial using experimental data from
#' an original randomized controlled trial, and adaptive inference strategies as described in
#' Hadad et al. (2021). Wraps around the internal implementation functions, and performs the full
#' MAB pipeline: preparing inputs, assigning treatments and imputing successes, and adaptively weighted
#' estimation.
#'
#' @param data A data.frame, data.table, or tibble containing input data from the trial. This should be the results
#' of a traditional Randomized Controlled Trial (RCT). data.frames will be converted to tibbles internally.
#'
#' @param time_unit A character string specifying the unit of time for assigning periods when `assignment_method` is "Date".
#' Acceptable values are "Day", "Week", or "Month". "Month" is a special case that is useful when an experiment
#' defines the months differently then the genuine dates, i.e. an experiment considers August as starting
#' the in the second half of July, or when exact calender months are required for the periods, not just
#' a lengths of time in the month range. As such it requires an additional column to be provided,
#' with the exact month desired for each observation, and it treats each observation
#' as occurring on the first of that month. It is also useful when dates are not provided, here
#' synthetic dates should be created placing each observation on the first on its specified month.
#'
#' @param perfect_assignment A logical value; if TRUE, assumes perfect information for treatment assignment
#' (i.e., all outcomes are observed regardless of the date).
#' If FALSE, hides outcomes not yet theoretically observed, based
#' on the dates treatments would have been assigned for each wave.
#' This is useful when simulating batch-based assignment where treatments were assigned
#' on a given day whether or not all the information from a prior batch was available and
#' you have exact dates treatments were assigned.
#'
#' @param algorithm A character string specifying the MAB algorithm to use. Options are "Thompson" or "UCB1".
#'
#' @param period_length A numeric value of length 1; represents the length of each treatment period.
#' If assignment method is "Date", this refers to the
#' length of periods by your specified `time_unit` (i.e., if "Day", 10 would be 10 days).
#' If assignment method is "Batch", this refers to the number of people in each batch. This factor
#' contributes most to the computational cost of calling the function, as large batch sizes make each iteration of
#' the simulation run slower, while each additional period adds time because of the extra iterations.
#' If you have a large dataset, consider passing it as a data.table.
#'
#' @param prior_periods A numeric value of length 1, or the character string "All"; number of previous periods to use
#' in the treatment assignment model. This is used to implement the stationary/non-stationary bandit.
#' For example, a non-stationary bandit assumes the true probability of success for each treatment changes over time, so to
#' account for that, not all prior data should be used when making decisions because it could be "out of date".
#'
#' @param whole_experiment A logical value; if TRUE, uses all past experimental data for imputing outcomes.
#' If FALSE, uses only data available up to the current period. In large datasets or with a high number
#' of periods, setting this to FALSE can be more computationally intensive, though not a significant
#' contributor to total runtime.
#'
#' @param conditions A named character vector containing treatment conditions. The elements
#' of this vector should be the names of each treatment as seen in your data, so to create it you can simply call
#' `unique(df[[condition_col_name]])`. The names of each element are used to reference the contents but are not inherently important;
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
#' would receive each treatment and 25% of women the same.
#'
#' @param block_cols A character vector of variables to block by. This vector should not be named.
#'
#' @param assignment_method A character string; one of "Date", "Batch", or "Individual", to define the assignment into treatment waves. When using
#' "Batch" or "Individual", ensure your dataset is pre-arranged in the proper order observations should be considered so that
#' groups are assigned correctly. For "Date", observations will be considered in chronological order.
#' "Individual" assignment can be time-consuming for larger datasets.
#'
#' @param control_augment A numeric value ranging from 0 to 1; proportion of each wave guaranteed to receive the "Control" treatment.
#' Default is 0.
#'
#' @param verbose A logical value; whether or not to print intermediate messages. Default is FALSE.
#'
#' @param ndraws A logical value; If TRUE, numerical corrections are made to the input
#' vector of success and total trials to when the Thompson Sampling procedure returns a vector
#' of 0's or NaN's due to overflow. The vectors are divided by 2, maintaining the proportions of
#' success to total trials in each arm, but this may impact the variance of the distribution. This occurs
#' recursively a maximum of 50 times before throwing an error. By default this is FALSE.
#'
#' @returns An object of class `mab`, which is a named list containing:
#' \itemize{
#' \item `final_data`: The processed tibble or data.table, containing new columns pertaining to the results of the trial.
#' \item `bandits`: A tibble or data.table containing the UCB1 statistics or Thompson Sampling posterior distributions for each period.
#' \item `assignment_probs`: A tibble or data.table containing the probability of being assigned each treatment arm at a given period.
#' \item `estimates`: A tibble or data.table containing the
#' AIPW (Augmented Inverse Probability Weighting) treatment effect estimates and variances, and traditional
#' sample means and variances, for each treatment arm.
#' \item `settings`: A named list of the configuration settings used in the trial.
#' \item `original_data`: The original `data` object passed to the function (data.frame, tibble, or data.table).
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
#' Probabilities via the \href{https://cran.r-project.org/web/packages/randomizr/index.html}{randomizr}
#' package, or as the treatment with the highest UCB1 statistic, while implementing the specific
#' treatment blocking and control augmentation specified. More details on bandit algorithms can in
#' \href{https://doi.org/10.48550/arXiv.1402.6028}{Kuleshov and Precup 2014} and
#' \href{https://doi.org/10.48550/arXiv.1904.07272}{Slivkins 2024}. When control augmentation is used
#' it requires at least the specified proportion be allocated to the control group, at which case the
#' probabilities of being assigned to each treatment are adjusted to achieve this, while also ensuring
#' that no probability becomes negative. This is NOT a Contextual bandit,
#' and as such the package does not support the acceptance of any covariate information nor use
#' any when performing the bandit-based assignment.
#'
#' If `perfect_assignment` is FALSE, at this step, some of the successes may be masked, if they occurred after
#' the specified treatment assignment date for that given period, but these will be unmasked in later periods.
#'
#' After treatments are assigned, observations with new treatments have their outcomes imputed, once again
#' using the \href{https://cran.r-project.org/web/packages/bandit/index.html}{randomizr} package, with any
#' specified treatment blocking implemented. The probabilities of success used to impute,
#' are estimated via the grouped means of successes from the original data, either from the whole trial, or
#' up to that period, defined by `whole_experiment`. If `perfect_assignment` is FALSE, only for those
#' who switched treatments and changed to success have their success date imputed using a grouped average from the
#' treatment block in the period. However if an observation changed treatment, but had succeeded under the
#' original experiment, they did not have their date changed.
#'
#' At the end of the simulation the results are aggregated together to calculate the Adaptively Weighted
#' Augmented Inverse Probability Estimator (Hadad et al. 2021) using the mean and variance formulas provided, under
#' the constant allocation rate adaptive schema. These estimators are unbiased and asymptotically normal.
#'
#' This procedure has the potential to be computationally expensive and time-consuming. Performance
#' depends on the relative size of each period, number of periods, and overall size of the dataset. This function
#' supports  data.tables, which are used when passed, but otherwise a combination of dplyr and base R is used.
#' In general, smaller batches run faster under base R, while larger ones could benefit from the performance
#' and memory efficiencies provided by data.table. An example dataset with 3,520 observations under individual assignment
#' takes 20-30 seconds under Base R and 40-50 seconds under data.table. It has also been observed, that larger datasets
#' at least over a  3 million rows, can cause numerical instability in the calculations. Internal safeguards exist
#' to prevent this, but to best way to preempt any issues, is to set `prior_periods` to a low number.
#'
#' @seealso [multiple_mab_simulation()], [summary.mab()], [plot.mab()].
#' @references
#'
#' Hadad, Vitor, David A. Hirshberg, Ruohan Zhan, Stefan Wager, and Susan Athey. 2021.
#' “Confidence Intervals for Policy Evaluation in Adaptive Experiments.” Proceedings of the National Academy of Sciences of the United States of America 118
#' (15): e2014602118. \url{https://doi.org/10.1073/pnas.2014602118}.
#'
#' Kuleshov, Volodymyr, and Doina Precup. 2014. “Algorithms for Multi-Armed Bandit Problems.”
#' arXiv. \url{https://doi.org/10.48550/arXiv.1402.6028}.
#'
#' Loecher, Thomas Lotze and Markus. 2022.
#' “Bandit: Functions for Simple a/B Split Test and Multi-Armed Bandit Analysis.”
#' \url{https://cran.r-project.org/web/packages/bandit/index.html}.
#'
#' Offer‐Westort, Molly, Alexander Coppock, and Donald P. Green. 2021.
#' “Adaptive Experimental Design: Prospects and Applications in Political Science.”
#' American Journal of Political Science 65 (4): 826–44. \url{https://doi.org/10.1111/ajps.12597}.
#'
#' Slivkins, Aleksandrs. 2024. “Introduction to Multi-Armed Bandits.” arXiv. \url{https://doi.org/10.48550/arXiv.1904.07272}.
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
                                  time_unit = NULL,
                                  period_length = NULL,
                                  block_cols = NULL,
                                  verbose = FALSE,
                                  ndraws = NULL) {
  prepped <- pre_mab_simulation(
    data = data, assignment_method = assignment_method,
    algorithm = algorithm, conditions = conditions,
    prior_periods = prior_periods, perfect_assignment = perfect_assignment,
    whole_experiment = whole_experiment, blocking = blocking,
    block_cols = block_cols, data_cols = data_cols,
    control_augment = control_augment, time_unit = time_unit,
    period_length = period_length,
    verbose = verbose, ndraws = ndraws
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
    ndraws = ndraws
  )
  results$settings$original_data <- data

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

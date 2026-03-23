#' Print Generic For `mab`
#' @description
#' Custom Print Display for objects of `mab` class returned by [mab_from_rct()].
#' Prevents the large list from being printed directly, and provides
#' useful information about the settings of each trial.
#' @param x A `mab` class object created by [mab_from_rct()].
#' @param ... Further arguments passed to or from other methods.
#' @method print mab
#' @name print.mab
#' @returns Text summary of settings used for the Multi-Arm Bandit trial.
#' @details
#' The items used to create the text summary can be found in the settings
#' element of the output object.
#'
#' `...` is provided to be compatible with `print()`, but no other arguments
#' change the output.
#' @export
#' @examples
#' # Running a Trial
#' x <- single_mab_simulation(
#'   data = tanf,
#'   algorithm = "thompson",
#'   assignment_method = "batch",
#'   period_length = 1750,
#'   prior_periods = "All",
#'   blocking = FALSE,
#'   whole_experiment = TRUE,
#'   perfect_assignment = TRUE,
#'   data_cols = c(
#'     id_col = "ic_case_id",
#'     success_col = "success",
#'     condition_col = "condition"
#'   )
#' )
#' print(x)
print.mab <- function(x, ...) {
  print_mab(x)
  cat("----------------------------------------------------- \n")
}
#-------------------------------------------------------------------------------
#' Print Helper for `mab` and `multiple.mab`
#' @description Common items for the print generics for `mab` and `multiple.mab` classes
#' @name print_mab
#' @param mab A `mab` or `multiple.mab` object.
#' @returns Text summary of settings used for the Multi-Arm Bandit trial.
#' @keywords internal
print_mab <- function(mab) {
  settings <- mab$settings

  cat(
    "Summary for MAB Procedure: \n ----------------------------------------------------- \n"
  )

  cat("Bandit Algorithm:     ", settings$algorithm, "\n")
  cat("Control Augmentation: ", settings$control_augment, "\n")
  cat("Bandit Assignment:    ", 1 - settings$random_assign_prop, "\n")
  cat("Randomized Assignment:", settings$random_assign_prop, "\n")
  cat("Perfect Assignment:   ", settings$perfect_assignment, "\n")
  cat("Whole Experiment:     ", settings$whole_experiment, "\n")
  if (settings$blocking) {
    cat("Blocking Variables:   ", settings$block_cols, "\n")
  }
  cat("Assignment Method:    ", settings$assignment_method, "\n")

  if (settings$assignment_method %in% c("batch", "date")) {
    cat("Period Length:        ", settings$period_length)
  }
  if (settings$assignment_method == "batch") {
    cat(" People\n")
  }
  if (settings$assignment_method == "date") {
    cat("", settings$time_unit)
    if (settings$period_length > 1) {
      cat("s\n")
    } else {
      cat("\n")
    }
  }

  cat(
    "Total Periods:        ",
    max(mab$bandits$period_number),
    "periods\n"
  )
  cat("Prior Periods:        ", settings$prior_periods, "periods\n")
  cat("Number of Treatments: ", length(settings$conditions), "\n")
  if (settings$control_augment > 0) {
    cat("Control Group:        ", settings$control, "\n")
  }
}

#------------------------------------------------------------------------------
##' Summary Generic For `mab` Class
#' @description
#' Summarizes the Results of a Single Multi-Arm Bandit Trial. Provides
#' confidence intervals around the AIPW estimates, final calculations
#' of the Thompson sampling probabilities or UCB1 values, and the number of observations assigned for each arm.
#' @param object A `mab` class object created by [single_mab_simulation()].
#' @param level Numeric value of length 1; indicates confidence interval Width (i.e 0.90, 0.95, 0.99).
#' Defaults to 0.95.
#' @param ... Additional arguments.
#' @method summary mab
#' @export
#' @details
#' The confidence intervals applied follow a standard normal distribution
#' because it is assumed the AIPW estimators are asymptotically normal as shown
#' in \href{https://www.pnas.org/doi/full/10.1073/pnas.2014602118}{Hadad et al. (2021)}.
#'
#' `...` is provided to be compatible with `summary()`, the function
#' does not have any additional arguments.
#'
#' All of the data provided to create a table like this is present in the object
#' created by [single_mab_simulation()] but
#' this provides a simple shortcut, which is useful when testing many
#' different simulations.
#
#' @returns A tibble containing summary information from the trial with the columns:
#' \itemize{
#' \item `Treatment_Arm`: Contains the treatment condition.
#' \item `Probability_Of_Best_Arm`/`UCB1_Value`: Final Thompson sampling probabilities or UCB1 values for each treatment.
#' \item `estimated_probability_of_success`: The AIPW estimates for the probability of success for each treatment.
#' \item `SE`: The standard error for the AIPW estimates.
#' \item `lower_bound`: The lower bound on the normal confidence interval for the `estimated_probability_of_success`. Default is 95%.
#' \item `upper_bound`: The upper bound on the normal confidence interval for the `estimated_probability_of_success`. Default is 95%.
#' \item `num_assigned`: The number of observations assigned to each treatment under the simulated trial.
#' \item `level`: The confidence level for the confidence interval, default is 95%.
#' \item `periods`: The total number of periods of the simulation.
#' }
#' @references
#' Hadad, Vitor, David A. Hirshberg, Ruohan Zhan, Stefan Wager, and Susan Athey. 2021.
#' "Confidence Intervals for Policy Evaluation in Adaptive Experiments." \emph{Proceedings of the National Academy of Sciences of the United States of America} 118
#' (15): e2014602118. \doi{10.1073/pnas.2014602118}.
#'
#' @example inst/examples/summary.mab_example.R
summary.mab <- function(object, level = 0.95, ...) {
  check_level(level)
  periods <- max(object$bandits$period_number)
  col2 <- switch(object$settings$algorithm,
    "ucb1" = "UCB1_Value",
    "thompson" = "Probability_Of_Best_Arm"
  )
  estimates <- object$estimates |>
    dplyr::filter(estimator == "AIPW") |>
    dplyr::mutate(mab_condition = as.character(mab_condition))

  quantities <- get_assignment_quantities(object, object$settings$conditions)
  quantities <- tibble::as_tibble(quantities) |>
    dplyr::mutate(mab_condition = names(quantities))

  normalq <- abs(stats::qnorm((1 - level) / 2))

  object$bandits[periods, ] |>
    tidyr::pivot_longer(
      cols = -period_number,
      names_to = "Treatment_Arm",
      values_to = col2
    ) |>
    dplyr::select(-period_number) |>
    dplyr::left_join(estimates, by = c("Treatment_Arm" = "mab_condition")) |>
    dplyr::mutate(
      SE = sqrt(variance),
      lower_bound = mean - normalq * sqrt(variance),
      upper_bound = mean + normalq * sqrt(variance)
    ) |>
    dplyr::select(-variance, -estimator) |>
    dplyr::left_join(quantities, by = c("Treatment_Arm" = "mab_condition")) |>
    dplyr::rename(
      "estimated_probability_of_success" = "mean",
      "num_assigned" = "value"
    ) |>
    dplyr::mutate(
      level = level,
      periods = periods
    )
}
#------------------------------------------------------------------------------
#' Plot Generic for `mab` objects
#' @description Uses [ggplot2::ggplot()] to plot the results of a single
#' Multi-Arm-Bandit trial. Provides options to select the type of plot,
#' and to change how the plot looks. Objects created can be added to
#' with `+` like any other ggplot plot, but arguments to change
#' the underlying geom must be passed to the function initially.
#'
#' @method plot mab
#' @param x A `mab` class object created by [single_mab_simulation()]
#' @param type String; Type of plot requested; valid types are:
#' \itemize{
#' \item `arm`: Shows Thompson sampling probabilities or UCB1 values over the trial period.
#' \item `assign`: Shows cumulative assignment proportions over the trial period.
#' \item `estimate`: Shows AIPW estimates for success probability with
#' user specified normal confidence intervals based on their estimated variance.
#' }
#' @param save Logical; Whether or not to save the plot to disk; FALSE by default.
#' @param path String; File directory to save file if necessary.
#' @inheritParams summary.mab
#' @param ... Arguments to pass to `ggplot2::geom_*` function (e.g. `color`, `linewidth`, `alpha`, `bins` etc.).
#' @details
#' This function provides minimalist plots to quickly view the results of any
#' Multi-Arm-Bandit trial, and has the ability to be customized through the `...`
#' inside the call and `+` afterwards. However, all the data necessary is
#' provided in the output of [single_mab_simulation()] for extreme
#' customization or professional plots, it is highly recommended
#' to start completely from scratch and not use the generic.
#'
#' The confidence intervals applied follow a standard normal distribution
#' because it is assumed the AIPW estimators are asymptotically normal as shown
#' in \href{https://www.pnas.org/doi/full/10.1073/pnas.2014602118}{Hadad et al. (2021)}
#'
#' @references
#' Hadad, Vitor, David A. Hirshberg, Ruohan Zhan, Stefan Wager, and Susan Athey. 2021.
#' "Confidence Intervals for Policy Evaluation in Adaptive Experiments." \emph{Proceedings of the National Academy of Sciences of the United States of America} 118
#' (15): e2014602118. \doi{10.1073/pnas.2014602118}.
#' @export
#' @example inst/examples/plot.mab_example.R
#' @returns Minimal ggplot object, that can be customized and added to with `+` (to change `scales`, `labels`, `legend`, `theme`, etc.).

plot.mab <- function(x, type, level = .95, save = FALSE, path = NULL, ...) {
  rlang::check_installed("ggplot2")
  plot <- switch(type,
    "arm" = plot_arms(x = x, ...),
    "assign" = plot_assign(x = x, ...),
    "estimate" = plot_estimates(x = x, level = level, ...),
    rlang::abort("Invalid Type: Specify `arm`, `assign`, or `estimate`")
  )
  if (save) {
    ggplot2::ggsave(plot, filename = path)
  }
  return(plot)
}

#-------------------------------------------------------------------------------
#' @name plot_arms
#' @title Plot Treatment Arms Over Time
#' @description
#' Helper to [plot.mab()]; plots treatment arms over time.
#' @returns ggplot object
#' @param x A `mab` object passed from [plot.mab()]
#' @inheritParams plot.mab
#' @returns Minimal ggplot object, that can be customized and added to with `+` (to change `scales`, `labels`, `legend`, `theme`, etc.).
#' @keywords internal

plot_arms <- function(x, ...) {
  rlang::check_installed("ggplot2")
  data <- x$bandits
  periods <- max(data$period_number)

  if (x$settings$algorithm == "ucb1") {
    ylab <- "UCB1 Values"
    title <- "UCB1 Sampling Over Time"
  }
  if (x$settings$algorithm == "thompson") {
    ylab <- "Posterior Probability of Being Best Arm"
    title <- "Thompson Sampling Over Time"
  }

  data |>
    tidyr::pivot_longer(
      cols = -period_number,
      names_to = "condition",
      values_to = "probs"
    ) |>
    ggplot2::ggplot(ggplot2::aes(
      x = period_number,
      y = probs,
      color = condition
    )) +
    ggplot2::geom_line(...) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 1, 0.1),
      limits = range(0, 1)
    ) +
    ggplot2::labs(
      x = "Assignment Period",
      y = ylab,
      title = title,
      color = "Treatment Arm"
    ) +
    ggplot2::theme_minimal()
}

#' @name plot_assign
#' @title Plot Cumulative Assignment Probability Over Time
#' @returns ggplot object
#' @param x A `mab` object passed from [plot.mab()]
#' @inheritParams plot.mab
#' @returns Minimal ggplot object, that can be customized and added to with `+` (to change `scales`, `labels`, `legend`, `theme`, etc.).
#' @keywords internal
plot_assign <- function(x, ...) {
  data <- x$final_data
  cumulative_data <- data |>
    dplyr::select(mab_condition, period_number) |>
    dplyr::arrange(period_number) |>
    dplyr::group_by(mab_condition, period_number) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::mutate(n = n / nrow(data)) |>
    dplyr::group_by(mab_condition) |>
    dplyr::mutate(cum_n = cumsum(n))

  ggplot2::ggplot(
    cumulative_data,
    ggplot2::aes(x = period_number, y = cum_n, color = mab_condition)
  ) +
    ggplot2::geom_line(...) +
    ggplot2::labs(
      x = "Assignment Period",
      y = "Proportion of Data",
      title = "Cumulative Assignments Across Trial",
      color = "Treatment Arm"
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 1, 0.1),
      limits = range(0, 1)
    ) +
    ggplot2::theme_minimal()
}

#' @name plot_estimates
#' @title Plot AIPW Estimates
#' @inheritParams plot.mab
#' @description
#' Plot summary of AIPW estimates and variances for each treatment arm.
#' @returns Minimal ggplot object, that can be customized and added to with `+` (to change `scales`, `labels`, `legend`, `theme`, etc.).
#' @keywords internal
plot_estimates <- function(x, level = 0.95, ...) {
  rlang::check_installed("ggplot2")
  check_level(level)
  normalq <- abs(stats::qnorm((1 - level) / 2))

  x$estimates |>
    dplyr::filter(estimator == "AIPW") |>
    ggplot2::ggplot(ggplot2::aes(x = mean, y = mab_condition)) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(
        xmin = mean - normalq * sqrt(variance),
        xmax = mean + normalq * sqrt(variance)
      ),
      ...
    ) +
    ggplot2::labs(
      x = "Probability of Success (AIPW)",
      y = "Treatment Condition",
      title = "AIPW Estimated Success Probabilities"
    ) +
    ggplot2::theme_minimal()
}
#-------------------------------------------------------------------------------
#' Check Level
#' @description
#' Checking if the `level` argument in the S3 generic methods
#' is valid for a confidence interval.
#' @name check_level
#' @inheritParams plot.mab
#' @returns Throws an error if `level` is invalid, else does nothing.
#' @keywords internal
check_level <- function(level) {
  if (!is.numeric(level) || (level < 0 || level > 1)) {
    rlang::abort(c(
      "`level` must be a number between 0 and 1",
      "x" = paste0("You passed: ", level)
    ))
  }
}

#' Print Generic For `multiple.mab`
#' @description Custom Print Display for `multiple.mab` objects returned by [multiple_mab_simulation()].
#' Prevents the large list output from being printed directly, and provides
#' useful information about the settings for the trials.
#' @method print multiple.mab
#' @param x A `multiple.mab` class object created by [multiple_mab_simulation()].
#' @param ... Further arguments passed to or from other methods.
#' @returns Text summary of settings used for the Multi-Arm Bandit trials.
#' @details
#' The items used to create the text summary can be found in the settings
#' element of the output object.
#'
#' `...` is provided to be compatible with `print()`, no other arguments
#' change output.
#' @export
#' @examples
#' # Running Multiple Simulations
#' x <- multiple_mab_simulation(
#'   data = tanf,
#'   algorithm = "thompson",
#'   assignment_method = "Batch",
#'   period_length = 1750,
#'   prior_periods = "All",
#'   blocking = FALSE,
#'   whole_experiment = TRUE,
#'   perfect_assignment = TRUE,
#'   data_cols = c(
#'     id_col = "ic_case_id",
#'     success_col = "success",
#'     condition_col = "condition"
#'   ),
#'   times = 5, seeds = sample.int(5)
#' )
#' print(x)
print.multiple.mab <- function(x, ...) {
  settings <- x$settings
  print_mab(x)
  cat("Trials Conducted:     ", settings$trials, "trials\n")
  cat("Keep Final Data:      ", settings$keep_data, "\n")
  cat("----------------------------------------------------- \n")
}
#------------------------------------------------------------------------------
#' Summary Generic For `multiple.mab` Class
#' @description
#' Summarizes results of multiple Multi-Arm Bandit Trials. Provides empirically estimated
#' and normally approximated confidence intervals on AIPW estimates for probability of success,
#' the number of times each arm was the chosen as the best treatment across all simulations, and the average for how many
#' units were assigned to each treatment across all the simulations.
#' @param object A `multiple.mab` object created by [multiple_mab_simulation].
#' @param level Numeric value of length 1; indicates confidence interval Width (i.e 0.90, 0.95, 0.99).
#' Defaults to 0.95.
#' @param ... Additional arguments.
#' @method summary multiple.mab
#' @details
#' The empirically estimated variances and confidence intervals, use the variance
#' measured directly in the AIPW estimates for each treatment over all the simulations.
#' The normal confidence intervals are estimated using an average of the measured variances
#' across the simulations.
#'
#' The best arm at the end of each trial is chosen by the highest UCB1 value or Thompson sampling
#' probability. These values indicate which treatment would be chosen next, or have the highest probability
#' of being chosen next, therefore representing the current best treatment.
#'
#' Additionally, an average and standard deviation for the number of units assigned to each
#' treatment across all the simulations is provided.
#'
#' `...` is provided to be compatible with `summary()`, the function
#' does not have any additional arguments.
#'
#' @example inst/examples/summary.multiple.mab_example.R
#' @returns A tibble containing summary information from the repeated trials with the columns:
#' \itemize{
#' \item `Treatment_Arm`: Contains the treatment condition.
#' \item `average_probability_of_success`: The average of the AIPW estimates for the probability of success for each treatment across the trials.
#' \item `SE_avg`: The standard error for the AIPW estimates, calculated as the square root of the average of the variances.
#' \item `SE_empirical`: The standard error estimated empirically as the standard deviation of the all the calculated AIPW estimates for probability of success.
#' \item `lower_normal`: The lower bound on the normal confidence interval for the `estimated_probability_of_success`. Default is 95%.
#' \item `upper_normal`: The upper bound on the normal confidence interval for the `estimated_probability_of_success`. Default is 95%.
#' \item `lower_empirical`: The lower bound on the empirical confidence interval for the `estimated_probability_of_success`. Calculated using the observed distribution
#' of AIPW estimated probabilities of success. Default is 95%.
#' \item `upper_empirical`: The upper bound on the empirical confidence interval for the `estimated_probability_of_success`. Calculated using the observed distribution
#' of AIPW estimated probabilities of success. Default is 95%.
#' \item `times_best`: The number of times each treatment arm was selected as the best for an individual simulation.
#' \item `average_num_assigned`: The average number of observations assigned to each treatment under the simulated trials.
#' \item `sd_num_assigned`: The standard deviation for the number of observations assigned to each treatment under the simulated trials.
#' \item `level`: The confidence level for the confidence interval, default is 95%.
#' }
#' @export

summary.multiple.mab <- function(object, level = 0.95, ...) {
  check_level(level)
  lower_level <- (1 - level) / 2
  upper_level <- 1 - lower_level

  quantities <- object$assignment_quantities |>
    tidyr::pivot_longer(
      cols = !trial,
      names_to = "mab_condition",
      values_to = "value"
    ) |>
    dplyr::group_by(mab_condition) |>
    dplyr::summarize(mean = mean(value), standard_dev = stats::sd(value))

  quantiles <- object$estimates |>
    dplyr::filter(estimator == "AIPW") |>
    dplyr::group_by(mab_condition, estimator) |>
    dplyr::summarize(
      lower = stats::quantile(mean, lower_level),
      upper = stats::quantile(mean, upper_level)
    ) |>
    dplyr::mutate(mab_condition = as.character(mab_condition))

  estimate <- object$estimates |>
    dplyr::filter(estimator == "AIPW") |>
    dplyr::group_by(mab_condition, estimator) |>
    dplyr::summarize(
      estimate_avg = mean(mean, na.rm = TRUE),
      SE_avg = sqrt(mean(variance, na.rm = TRUE)),
      SE_empirical = stats::sd(mean),
      .groups = "drop",
    ) |>
    dplyr::mutate(
      lower = estimate_avg + stats::qnorm(lower_level) * SE_avg,
      upper = estimate_avg + stats::qnorm(upper_level) * SE_empirical,
      mab_condition = as.character(mab_condition)
    ) |>
    dplyr::left_join(
      quantiles,
      by = c("mab_condition", "estimator"),
      suffix = c("_normal", "_empirical")
    )

  bandits <- object$bandits |>
    dplyr::group_by(trial) |>
    dplyr::filter(period_number == max(period_number)) |>
    tidyr::pivot_longer(
      cols = c(-trial, -period_number),
      names_to = "mab_condition",
      values_to = "bandit"
    ) |>
    dplyr::slice_max(order_by = bandit) |>
    dplyr::ungroup() |>
    dplyr::count(mab_condition)

  summary <- dplyr::left_join(estimate, bandits, by = c("mab_condition")) |>
    dplyr::rename(times_best = "n") |>
    dplyr::mutate(
      times_best = dplyr::if_else(is.na(times_best), 0, times_best),
      level = level
    ) |>
    dplyr::select(-estimator) |>
    dplyr::rename(
      "average_probability_of_success" = "estimate_avg",
      Treatment_Arm = mab_condition
    ) |>
    dplyr::left_join(quantities, by = c("Treatment_Arm" = "mab_condition")) |>
    dplyr::rename(average_num_assigned = mean, sd_num_assigned = standard_dev)
  return(summary)
}

#' Plot Generic For `multiple.mab` Objects
#' @description Uses [ggplot2::ggplot()] to plot the results of multiple
#' Multi-Arm-Bandit trials.
#'
#' @method plot multiple.mab
#' @param x A `multiple.mab` class object created by [multiple_mab_simulation()].
#' @param type String; Type of plot requested; valid types are:
#' \itemize{
#' \item `summary`: Shows the number of times each arm was selected as the highest chance of being the best.
#' \item `hist`: Shows histograms for each treatment condition's proportion of success across trials or number of obersvations assigned.
#' \item `estimate`: Shows proportion of success AIPW estimates using specified normal or empirical confidence intervals.
#' }
#' @param quantity The quantities to plot when `type = "hist"`, accepts either 'estimate' to plot the distributuons of the AIPW estimates, or
#' 'assignment' to plot the distributions of the number of observations assigned to each treatment across the repeated trials.
#' @param save Logical; Whether or not to save the plot to disk; `FALSE` by default.
#' @param path String; File directory to save file.
#' @param ... Arguments to pass to `ggplot2::geom_*` function (e.g. `color`, `linewidth`, `alpha`, `bins` etc.). In the case of `type = "hist"`, additional
#' arguments must be passed in to distinct lists, one named `geom` which are passed to `ggplot2::geom_*`
#' and one named `facet` which are passed to `ggplot2::facet_grid`.
#' @param cdf String; specifies the type of CDF to use when analyzing the estimates.
#' valid CDFs are the 'empirical' CDF, the 'normal' CDF. Used when type = `estimate`. The 'normal' CDF uses the fact
#' that the AIPW estimates are asymptotically normal, while the empirical CDF (eCDF) estimates the CDF from the sample
#' of AIPW estimates.
#' @inheritParams summary.multiple.mab
#' @details
#' This function provides minimalist plots to quickly view the results of the procedure
#' and has the ability to be customized through the `...`
#' in the call and `+` afterwords. However, all the data necessary is
#' provided in the output of [multiple_mab_simulation()] for extreme
#' customization or professional plots, it is highly recommended
#' to start completely from scratch and not use the generic.
#'
#' @example inst/examples/plot.multiple.mab_example.R
#' @export
#' @returns Minimal ggplot object, that can be customized and added to with `+` (to change `scales`, `labels`, `legend`, `theme`, etc.).

plot.multiple.mab <- function(
  x,
  type,
  quantity,
  cdf = NULL,
  level = 0.95,
  save = FALSE,
  path = NULL,
  ...
) {
  rlang::check_installed("ggplot2")
  plot <- switch(type,
    "summary" = plot_summary(x = x, ...),
    "hist" = plot_hist(
      x = x,
      quantity,
      params = rlang::dots_list(..., .named = TRUE)
    ),
    "estimate" = plot_mult_estimates(x = x, cdf = cdf, level = level, ...),
    rlang::abort(
      "Invalid Type: Valid types are `hist`, `summary`, estimate`."
    )
  )

  if (save) {
    ggplot2::ggsave(plot, filename = path)
  }

  return(plot)
}
#-------------------------------------------------------------------------------
#' @name plot_summary
#' @title Plot Treatment Arms Over Multiple Trials
#' @description
#' Plots summary results for [plot.multiple.mab()], shows then number of times each arm was selected as the best in a bar chart.
#' @inheritParams plot.multiple.mab
#' @returns Minimal ggplot object, that can be customized and added to with `+` (to change `scales`, `labels`, `legend`, `theme`, etc.).
#' @keywords internal

plot_summary <- function(x, ...) {
  rlang::check_installed("ggplot2")
  summary(x) |>
    ggplot2::ggplot(ggplot2::aes(x = Treatment_Arm, y = times_best)) +
    ggplot2::geom_bar(stat = "identity", ...) +
    ggplot2::labs(
      x = "Treatment Arm",
      y = "Times Selected as Best Arm",
      title = "Treatment Arm Success Over Repeated Trials"
    ) +
    ggplot2::theme_minimal()
}


#------------------------------------------------------------------------------
#
#' @name plot_hist
#' @title Plots Histograms of [multiple_mab_simulation()] Results
#' @description
#' Plots distribution of AIPW estimates over trials for [plot.multiple.mab()] or the distribution of the number of observations assigned to each treatment arm.
#' @inheritParams plot.multiple.mab
#' @param params The dynamic dots (`...`) from [plot.multiple.mab()] should be a named list containing two elements, `geom` and `facet` containing arguments for
#' `ggplot2::geom_histogram()` and `ggplot2::facet_grid()` respectively.
#' @returns Minimal ggplot object, that can be customized and added to with `+` (to change `scales`, `labels`, `legend`, `theme`, etc.).
#' @keywords internal
plot_hist <- function(x, quantity, params) {
  rlang::check_installed("ggplot2")
  data <- switch(quantity,
    "estimate" = {
      x$estimates
    },
    "assignment" = {
      x$assignment_quantities |>
        tidyr::pivot_longer(
          cols = !trial,
          names_to = "mab_condition",
          values_to = "mean"
        )
    },
    rlang::abort(
      "Invalid `quantity`, valid values are 'estimate' and 'assignment'"
    )
  )
  plot_labels <- switch(quantity,
    "estimate" = list(
      x = "Estimate",
      title = "Estimate Distributions Across Trials"
    ),
    "assignment" = list(
      x = "Number of People Assigned",
      title = "Assignment Distributions Across Trials"
    )
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = mean, y = ggplot2::after_stat(density))
  ) +
    rlang::exec(ggplot2::geom_histogram, !!!(params$geom)) +
    rlang::exec(
      ggplot2::facet_grid,
      !!!(c(~mab_condition, params$facet))
    ) +
    ggplot2::labs(
      x = plot_labels$x,
      y = "Density",
      title = plot_labels$title
    ) +
    ggplot2::theme_minimal()

  return(plot)
}

#-------------------------------------------------------------------------------
#' @name plot_mult_estimates
#' @title Plots AIPW Confidence Intervals
#' @description
#' Plots the uncertainty AIPW estimates for each arm using the specified variance from the repeated trials for [plot.multiple.mab()].
#' @inheritParams plot.multiple.mab
#' @returns Minimal ggplot object, that can be customized and added to with `+` (to change `scales`, `labels`, `legend`, `theme`, etc.).
#' @keywords internal

plot_mult_estimates <- function(x, cdf, level, ...) {
  rlang::check_installed("ggplot2")
  check_level(level)
  if (is.null(cdf)) {
    rlang::abort("Invalid CDF: Valid CDF's are, empirical`, and `normal`")
  }
  cols <- switch(cdf,
    "empirical" = c("upper_empirical", "lower_empirical"),
    "normal" = c("upper_normal", "lower_normal"),
    rlang::abort("Invalid `CDF`: valid CDFs are `normal` or `empirical`")
  )

  summary(x, level = level) |>
    dplyr::select(
      !!!rlang::syms(cols),
      Treatment_Arm,
      average_probability_of_success
    ) |>
    ggplot2::ggplot(ggplot2::aes(
      x = average_probability_of_success,
      y = Treatment_Arm
    )) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(
        xmax = !!rlang::sym(cols[[1]]),
        xmin = !!rlang::sym(cols[[2]])
      ),
      ...
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Probability of Succcess (AIPW)",
      y = "Treatment Arm",
      title = "Uncertainy Around Treatment Arm Estimates"
    )
}

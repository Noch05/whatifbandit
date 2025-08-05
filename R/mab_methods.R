#' Print Generic For `mab`
#' @description
#' Custom Print Display for objects of `mab` class returned by [single_mab_simulation()].
#' Prevents the large list from being printed directly to the R console, and provides
#' useful information about the settings of each trial.
#' @param x `mab` class object created by [single_mab_simulation()]
#' @param ... further arguments passed to or from other methods
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
#'   algorithm = "Thompson",
#'   assignment_method = "Batch",
#'   period_length = 1750,
#'   conditions = as.character(levels(tanf$condition)),
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
  base::cat("----------------------------------------------------- \n")
}
#-------------------------------------------------------------------------------
#' Print Helper for `mab` and `multiple.mab`
#' @description Common items for the print generics for `mab` and `multiple.mab` classes
#' @name print_mab
#' @param mab `mab` or `multiple.mab` object to derive settings from
#' @returns Text summary of settings used for the Multi-Arm Bandit trial.
#' @noRd
print_mab <- function(mab) {
  settings <- mab$settings

  base::cat("Summary for MAB Procedure: \n ----------------------------------------------------- \n")

  base::cat("Bandit Algorithm:     ", settings$algorithm, "\n")
  base::cat("Control Augmentation: ", settings$control_augment, "\n")
  base::cat("Bandit Assignment:    ", 1 - settings$random_assign_prop, "\n")
  base::cat("Randomized Assignment:", settings$random_assign_prop, "\n")
  base::cat("Perfect Assignment:   ", settings$perfect_assignment, "\n")
  base::cat("Whole Experiment:     ", settings$whole_experiment, "\n")
  base::cat("Blocking Variables:   ", settings$block_cols, "\n")
  base::cat("Assignment Method:    ", settings$assignment_method, "\n")

  if (settings$assignment_method %in% c("Batch", "Date")) {
    base::cat("Period Length:        ", settings$period_length)
  }
  if (settings$assignment_method == "Batch") {
    base::cat(" People\n")
  }
  if (settings$assignment_method == "Date") {
    base::cat("", settings$time_unit)
    if (settings$period_length > 1) {
      base::cat("s\n")
    } else {
      base::cat("\n")
    }
  }

  base::cat("Total Periods:        ", length(mab$bandits), "periods\n")
  base::cat("Prior Periods:        ", settings$prior_periods, "periods\n")
  base::cat("Treatments:           ", length(settings$conditions), "treatment arms\n")
}

#------------------------------------------------------------------------------
##' Summary Generic for "mab" class
#' @description
#' Summarizes the Results of a Single Multi-Arm Bandit Trial. Provides
#' confidence intervals around the AIPW estimates, and the final calculations
#' of the Thompson Probabilities or UCB1 statistics for each arm.
#' @param object `mab`` class object created by [single_mab_simulation()].
#' @param level Numeric value of length 1; indicates confidence interval Width (i.e 0.90, .95, 0.99).
#' Defaults to 0.95
#' @param ... additional arguments.
#' @method summary mab
#' @export
#' @details
#' The confidence intervals applied follow a standard normal distribution
#' because it is assumed the AIPW estimators are asymptotically normal as shown
#' in \href{https://www.pnas.org/doi/full/10.1073/pnas.2014602118}{Hadad et al. (2021)}
#'
#' `...` is provided to be compatible with `summary()`, the function
#' does not have any additional arguments.
#'
#' All of the data provided to create a table like this is present in the object
#' created by [single_mab_simulation()] but
#' this provides a simple shortcut, which is useful when testing many
#' different simulations.
#' @returns Tibble containing summary information from the trial.
#' @references
#' Hadad, Vitor, David A. Hirshberg, Ruohan Zhan, Stefan Wager, and Susan Athey. 2021.
#' “Confidence Intervals for Policy Evaluation in Adaptive Experiments.” \emph{Proceedings of the National Academy of Sciences of the United States of America} 118
#' (15): e2014602118. \doi{10.1073/pnas.2014602118}.
#'
#' @example inst/examples/summary.mab_example.R
summary.mab <- function(object, level = 0.95, ...) {
  check_level(level)
  periods <- base::max(object$bandits$period_number)
  col2 <- switch(object$settings$algorithm,
    "UCB1" = "UCB1_Statistic",
    "Thompson" = "Probability_Of_Best_Arm"
  )
  estimates <- object$estimates |>
    dplyr::filter(estimator == "AIPW")

  normalq <- base::abs(stats::qnorm((1 - level) / 2))

  object$bandits[periods, ] |>
    tidyr::pivot_longer(
      cols = -period_number, names_to = "Treatment_Arm",
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
    dplyr::rename("AIPW" = "mean") |>
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
#' @param x `mab` class object created by [single_mab_simulation()]
#' @param type String; Type of plot requested; valid types are:
#' \itemize{
#' \item `arm`: Shows Thompson Probability or UCB1 Statistic over the trial period.
#' \item `assign`: Shows Assignment Probability/Proportion over trial period.
#' \item `estimate`: Shows proportion of success estimates with user
#' specified normal confidence intervals based on their estimated variance.
#' }
#' @param save Logical; Whether or not to save the plot to disk; FALSE by default.
#' @param path String; File directory to save file.
#' @inheritParams summary.mab
#' @param estimator Estimator to plot; Either "AIPW", "Sample" or "Both"; only used by "estimate" type.
#' @param ... arguments to pass to `ggplot2:geom_*` function (e.g. `color`, `linewidth`, `alpha`, etc.)
#' @details
#' The plot generic requires \href{https://cran.r-project.org/package=ggplot2}{ggplot2}
#' which is not required by the package, so it must be installed separately.
#'
#' This function provides minimalist plots to quickly view the results of any
#' Multi-Arm-Bandit trial, and has the ability to be customized through the `...`
#' in the call and `+` afterwords. However, all the data necessary is
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
#' “Confidence Intervals for Policy Evaluation in Adaptive Experiments.” \emph{Proceedings of the National Academy of Sciences of the United States of America} 118
#' (15): e2014602118. \doi{10.1073/pnas.2014602118}.
#' @export
#' @example inst/examples/plot.mab_example.R
#' @returns Minimal ggplot object, that can be customized and added to with `+` (To change, scales, labels, legend, theme, etc.)

plot.mab <- function(x, type, estimator = NULL, level = .95, save = FALSE, path = NULL, ...) {
  rlang::check_installed("ggplot2")
  plot <- switch(type,
    "arm" = plot_arms(x = x, object = "bandits", ...),
    "assign" = plot_arms(x = x, object = "assignment_probs", ...),
    "estimate" = plot_estimates(x = x, estimator = estimator, level = level, ...),
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
#' Helper to [plot.mab()]. Plots Treatment Arms over Time.
#' @returns ggplot object
#' @param x, mab object passed from [plot.mab()]
#' @inheritParams plot.mab
#' @param object, String; Location to gather treatment arm data from, either
#' "bandits" or "assignment_probs".
#' @returns Minimal ggplot object, that can be customized and added to with `+` (To change, scales, labels, legend, theme, etc.)
#' @keywords internal

plot_arms <- function(x, object, ...) {
  rlang::check_installed("ggplot2")
  data <- x[[object]]
  periods <- base::max(data$period_number)

  if (object == "bandits") {
    if (x$settings$algorithm == "UCB1") {
      ylab <- "UCB1 Statistic"
      title <- "UCB1 Sampling Over Time"
    }
    if (x$settings$algorithm == "Thompson") {
      ylab <- "Posterior Probability of Being Best Arm"
      title <- "Thompson Sampling Over Time"
    }
  }
  if (object == "assignment_probs") {
    ylab <- "Probability of Assignment"
    title <- "Assignment Probabilities Over Time"
  }
  data |>
    tidyr::pivot_longer(
      cols = -period_number,
      names_to = "condition",
      values_to = "probs"
    ) |>
    ggplot2::ggplot(ggplot2::aes(
      x = period_number, y = probs,
      color = condition
    )) +
    ggplot2::geom_line(...) +
    ggplot2::scale_y_continuous(breaks = base::seq(0, 1, 0.1), limits = base::range(0, 1)) +
    ggplot2::scale_x_continuous(
      breaks = base::seq(0, periods, 1),
      limits = range(1, periods)
    ) +
    ggplot2::labs(
      x = "Assignment Period",
      y = ylab,
      title = title,
      color = "Treatment Arm"
    ) +
    ggplot2::theme_minimal()
}

#' @name plot_estimates
#' @title Plot AIPW/Sample Estimates
#' @inheritParams plot.mab
#' @description
#' Plot Summary of AIPW estimates and variances for Each Treatment Arm.
#' @returns Minimal ggplot object, that can be customized and added to with `+` (To change, scales, labels, legend, theme, etc.)`
#' @keywords internal
plot_estimates <- function(x, estimator, level = 0.95, ...) {
  rlang::check_installed("ggplot2")
  check_level(level)
  estimator_arg <- check_estimator(estimator)
  normalq <- base::abs(stats::qnorm((1 - level) / 2))

  x$estimates |>
    dplyr::filter(estimator %in% estimator_arg) |>
    ggplot2::ggplot(ggplot2::aes(x = mean, y = mab_condition)) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = mean - normalq * sqrt(variance), xmax = mean + normalq * sqrt(variance)),
      ...
    ) +
    ggplot2::scale_x_continuous(breaks = base::seq(0, 1, 0.05), limits = base::range(0, 1)) +
    ggplot2::facet_wrap(~estimator) +
    ggplot2::labs(
      x = "Probability of Success (AIPW)",
      y = "Treatment Condition",
      title = "AIPW Estimates of Success"
    ) +
    ggplot2::theme_minimal()
}
#-------------------------------------------------------------------------------
#' Check Level
#' @description
#' Shorthand for Checking if the `level` argument in the S3 generic methods
#' is valid for a confidence interval.
#' @name check_level
#' @inheritParams plot.mab
#' @returns Throws an error if `level` is invalid, else does nothing.
#' @keywords internal
check_level <- function(level) {
  if (!is.numeric(level) || (level < 0 || level > 1)) {
    rlang::abort(c("`level` must be a number between 0 and 1",
      "x" = paste0("You passed: ", level)
    ))
  }
}

#' Check Estimator
#' @description
#' Shorthand for checking if the `estimator` passed to
#' [plot.mab()] and [plot.multiple.mab()] are valid.
#' @name check_estimator
#' @inheritParams plot.mab
#' @returns Throws an error if the argument is invalid; returns character vector
#' with the user's selection based on the argument.
#' @keywords internal
check_estimator <- function(estimator) {
  if (base::is.null(estimator)) {
    rlang::abort("Invalid Estimator: Valid Estimators are `both`, `AIPW`, and `Sample`")
  } else {
    estimator_arg <- switch(estimator,
      "both" = c("Sample", "AIPW"),
      "AIPW" = c("AIPW"),
      "Sample" = c("Sample"),
      rlang::abort("Invalid Estimator: Valid Estimators are `both`, `AIPW`, and `Sample`")
    )
  }
  return(estimator_arg)
}

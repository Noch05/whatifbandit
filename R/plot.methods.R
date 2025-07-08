#' Plot Generic for `mab` objects
#' @description Uses [ggplot2::ggplot()] to summarize the results of a single
#' Multi-Arm Bandit Trial
#'
#' @method plot mab
#' @param x `mab` class object created by [single_mab_simulation()]
#' @param type String; Type of plot requested; valid types are:
#' \itemize{
#' \item `arm`: Shows Thompson Probability or UCB1 Statistic over the trial period.
#' \item `assign`: Shows Assignment Probability/Proportion over trial period.
#' \item `estimate`: Shows proportion of success estimates with 95% Normal Confidence Intervals based on their estimated variance.
#' }
#' @param save Logical; Whether or not to save the plot to disk; FALSE by default.
#' @param path String; File directory to save file.
#' @inheritParams summary.mab
#' @param estimator Estimator to plot; Either "AIPW", "Sample" or "Both"; only used by "estimate" type
#' @param ... arguments to pass to `ggplot2:geom_*` function (e.g. `color`, `linewidth`, `alpha`, etc.)
#' @export
#' @returns Minimal ggplot object, that can be customized and added to with `+` (To change, scales, labels, legend, theme, etc.)

plot.mab <- function(x, type, estimator = NULL, level = .95, save = FALSE, path = NULL, ...) {
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
#' "bandits" or "assignment_probs"
#' @returns Minimal ggplot object, that can be customized and added to with `+` (To change, scales, labels, legend, theme, etc.)

plot_arms <- function(x, object, ...) {
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
#' Plot Summary of AIPW estimates and variances for Each Treatment Arm
#' @returns Minimal ggplot object, that can be customized and added to with `+` (To change, scales, labels, legend, theme, etc.)`
plot_estimates <- function(x, estimator, level = 0.95, ...) {
  if (base::is.null(estimator)) rlang::abort("Invalid Estimator: Valid Estimators are `both`, `AIPW`, and `Sample`")

  estimator_arg <- switch(estimator,
    "Both" = c("Sample", "AIPW"),
    "AIPW" = c("AIPW"),
    "Sample" = c("Sample"),
    rlang::abort("Invalid Estimator: Valid Estimators are `both`, `AIPW`, and `Sample`")
  )
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
#-------------------------------------------------------------------------------
#' Plot Generic for `multiple.mab` objects
#' @description Uses [ggplot2::ggplot()] to summarize the results of multiple
#' Multi-Arm Bandit Trials
#'
#' @method plot multiple.mab
#' @param x `multiple.mab` class object created by [multiple_mab_simulation()]
#' @param type String; Type of plot requested; valid types are:
#' \itemize{
#' \item `summary`: Shows the number of times each arm was selected as the highest chance of being the best.
#' \item `hist`: Shows histograms for each treatment condition's proportion of success across trials.
#' }
#' @param estimator Estimator to plot; Either "AIPW", "Sample" or "Both"; used by `hist` and `estimate`.
#' @param save Logical; Whether or not to save the plot to disk; FALSE by default.
#' @param path String; File directory to save file.
#' @param ... arguments to pass to `ggplot2:geom_*` function (e.g. `color`, `linewidth`, `alpha`, `bins` etc.)
#' @param cdf String; specifies the type of CDF to use when analyzing the estimates.
#' valid cdfs are the `empirical` cdf, the `normal` cdf. Used when type = `estimate`.
#' @inheritParams summary.multiple.mab
#' @export
#' @returns Minimal ggplot object, that can be customized and added to with `+` (To change, scales, labels, legend, theme, etc.)

plot.multiple.mab <- function(x, type, estimator = NULL, cdf = NULL, level = 0.95, save = FALSE, path = NULL, ...) {
  plot <- switch(type,
    "summary" = plot_summary(x = x, ...),
    "hist" = plot_hist(x = x, estimator = estimator, ...),
    "estimate" = plot_mult_estimates(
      x = x, estimator = estimator, cdf = cdf,
      level = level, ...
    ),
    rlang::abort("Invalid Type: Valid types are `hist`, `summary`, estimate`.")
  )

  if (save) {
    ggplot2::ggsave(plot, filename = path)
  }

  return(plot)
}
#-------------------------------------------------------------------------------
#' @name plot_summary
#' @title Plot treatment Arms over multiple trials
#' @description
#' Plots Summary Results for [plot.multiple.mab()]
#' @inheritParams plot.multiple.mab
#' @returns Minimal ggplot object, that can be customized and added to with `+` (To change, scales, labels, legend, theme, etc.)

plot_summary <- function(x, ...) {
  summary(x) |>
    dplyr::filter(estimator == "AIPW") |>
    ggplot2::ggplot(ggplot2::aes(x = mab_condition, y = times_best)) +
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
#' @title Plots Distribution of AIPW and Sample estimates over trials
#' @description
#' Plots Distribution of AIPW and Sample estimates over trials for [plot.multiple.mab()]
#' @inheritParams plot.multiple.mab
#' @returns Minimal ggplot object, that can be customized and added to with `+` (To change, scales, labels, legend, theme, etc.)
plot_hist <- function(x, estimator, ...) {
  if (base::is.null(estimator)) rlang::abort("Invalid Estimator: Valid Estimators are `both`, `AIPW`, and `Sample`")

  estimator_arg <- switch(estimator,
    "Both" = c("Sample", "AIPW"),
    "AIPW" = c("AIPW"),
    "Sample" = c("Sample"),
    rlang::abort("Invalid Estimator: Valid Estimators are `both`, `AIPW`, and `Sample`")
  )

  x$estimates |>
    dplyr::filter(estimator %in% estimator_arg) |>
    ggplot2::ggplot(ggplot2::aes(x = mean, y = ggplot2::after_stat(stats::density))) +
    ggplot2::geom_histogram(...) +
    ggplot2::facet_grid(~ mab_condition + estimator) +
    ggplot2::labs(
      x = "Estimate", y = "Density",
      title = "Estimate Distributions Across Trials"
    ) +
    ggplot2::theme_minimal()
}

#-------------------------------------------------------------------------------
#' @name plot_mult_estimates
#' @title Plots AIPW/Sample Estimates for each Arm
#' @description
#' Plots AIPW/Sample Estimates for each arm using variance from the repeated trials.
#' @inheritParams plot.multiple.mab
#' @returns Minimal ggplot object, that can be customized and added to with `+` (To change, scales, labels, legend, theme, etc.)


plot_mult_estimates <- function(x, estimator, cdf, level, ...) {
  if (base::is.null(estimator)) {
    rlang::abort("Invalid Estimator: Valid Estimators are `both`, `AIPW`, and `Sample`")
  }

  estimator_arg <- switch(estimator,
    "Both" = c("Sample", "AIPW"),
    "AIPW" = c("AIPW"),
    "Sample" = c("Sample"),
    rlang::abort("Invalid Estimator: Valid Estimators are `both`, `AIPW`, and `Sample`")
  )

  if (base::is.null(cdf)) {
    rlang::abort("Invalid Estimator: Valid Estimators are, empirical`, and `normal`")
  }
  cols <- switch(cdf,
    ,
    "empirical" = c("upper_empirical", "lower_empirical"),
    "normal" = c("upper_normal", "lower_normal")
  )

  summary(x, level = level) |>
    dplyr::filter(estimator %in% estimator_arg) |>
    dplyr::select(tidyselect::all_of(cols), estimator, mab_condition, estimate_avg) |>
    ggplot2::ggplot(ggplot2::aes(x = estimate_avg, y = mab_condition)) +
    ggplot2::geom_errorbarh(ggplot2::aes_string(xmin = cols[[1]], xmax = cols[[2]])) +
    ggplot2::facet_grid(~estimator) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Estimate", y = "Treatment Arm",
      title = "Uncertainy Around Treatment Arm Estimates"
    ) +
    ggplot2::theme(panel.spacing = ggplot2::unit(2, "lines"))
}

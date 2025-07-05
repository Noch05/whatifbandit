#' Plot Generic for mab objects
#' @description Uses [ggplot2::ggplot()] to summarize the results of a single
#' Multi-Arm Bandit Trial
#'
#' @method plot mab
#' @param x mab class object created by
#' @param type String; Type of plot requested; valid types are:
#' \itemize{
#' \item {arm}:  Shows Thompson Probability or UCB1 Statistic over the trial period.
#' \item {assign}:  Shows Assignment Probability/Proportion over trial period.
#' \item {estimate}: Shows AIPW Estimates with 95% Normal Confidence Intervals based on their estimated variance.
#' }
#' @param save Logical; Whether or not to save the plot to disk; FALSE by default.
#' @param path String; File directory to save file.
#' @param estimator Estimator to plot; Either "AIPW", "Sample" or "Both"; only used by "estimate" type
#' @param ... arguments to pass to `ggplot2:geom_*` function (e.g. `color`, `linewidth`, `alpha`, etc.)
#' @export
#' @returns ggplot object, that can be customized and added to with `+` (To change, scales, labels, legend, theme, etc.)

plot.mab <- function(x, type, estimator = NULL, save = FALSE, path = NULL, ...) {
  plot <- switch(type,
    "arm" = plot_arms(x = x, object = "bandits", ...),
    "assign" = plot_arms(x = x, object = "assignment_probs", ...),
    "estimate" = plot_estimates(x = x, estimator = estimator, ...),
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
#' @title Plot AIPW Estimates
#' @inheritParams plot.mab
#' @description
#' Plot Summary of AIPW estimates and variances for Each Treatment Arm
#' @returns ggplot2 object that can be added to with `+`
plot_estimates <- function(x, estimator, ...) {
  if (base::is.null(estimator)) rlang::abort("Invalid Estimator: Valid Estimators are `both`, `AIPW`, and `Sample`")

  estimator_arg <- switch(estimator,
    "Both" = c("Sample", "AIPW"),
    "AIPW" = c("AIPW"),
    "Sample" = c("Sample"),
    rlang::abort("Invalid Estimator: Valid Estimators are `both`, `AIPW`, and `Sample`")
  )


  x$estimates |>
    dplyr::filter(estimator %in% estimator_arg) |>
    ggplot2::ggplot(ggplot2::aes(x = mean, y = mab_condition)) +
    ggplot2::geom_pointrange(
      ggplot2::aes(xmin = mean - 1.96 * sqrt(variance), xmax = mean + 1.96 * sqrt(variance)),
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

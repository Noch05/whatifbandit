#' Print Generic For `multiple.mab`
#' @description Custom Print Display for `multiple.mab`` objects returned by [multiple_mab_simulation()].
#' @method print multiple.mab
#' @param x `multiple.mab` class object
#' @param ... further arguments passed to or from other methods
#' @returns Text summary of settings used for the Multi-Arm Bandit trials.
#' @export
print.multiple.mab <- function(x, ...) {
  settings <- x$settings
  print_mab(x)
  base::cat("Trials Conducted:     ", settings$trials, "trials\n")
  base::cat("Keep Final Data:      ", settings$keep_data, "\n")
  base::cat("----------------------------------------------------- \n")
}
#------------------------------------------------------------------------------
#' Summary Generic for "multiple.mab" class
#' @description
#' Summarizes results of of multiple Multi-Arm Bandit Trials
#' @param object `multiple.mab` object created by [multiple_mab_simulation]
#' @param level Confidence Interval Width (i.e 0.90, .95, 0.99)
#' @param ... additional arguments.
#' @method summary multiple.mab
#' @example inst/examples/summary.multiple.mab_example.R
#' @export

summary.multiple.mab <- function(object, level = 0.95, ...) {
  check_level(level)
  lower_level <- (1 - level) / 2
  upper_level <- 1 - lower_level

  quantiles <- object$estimates |>
    dplyr::group_by(mab_condition, estimator) |>
    dplyr::summarize(
      lower = stats::quantile(mean, lower_level),
      upper = stats::quantile(mean, upper_level)
    )

  estimate <- object$estimates |>
    dplyr::group_by(mab_condition, estimator) |>
    dplyr::summarize(
      estimate_avg = base::mean(mean, na.rm = TRUE),
      variance_avg = base::mean(variance, na.rm = TRUE),
      variance_resample = stats::var(mean), .groups = "drop",
    ) |>
    dplyr::mutate(
      lower = estimate_avg + stats::qnorm(lower_level) * base::sqrt(variance_avg),
      upper = estimate_avg + stats::qnorm(upper_level) * base::sqrt(variance_avg)
    ) |>
    dplyr::left_join(quantiles, by = c("mab_condition", "estimator"), suffix = c("_normal", "_empirical"))

  bandits <- object$bandits |>
    dplyr::group_by(trial) |>
    dplyr::filter(period_number == max(period_number)) |>
    tidyr::pivot_longer(
      cols = c(-trial, -period_number), names_to = "mab_condition",
      values_to = "bandit"
    ) |>
    dplyr::slice_max(order_by = bandit) |>
    dplyr::ungroup() |>
    dplyr::count(mab_condition)

  summary <- dplyr::left_join(estimate, bandits, by = c("mab_condition")) |>
    dplyr::rename(times_best = "n") |>
    dplyr::mutate(
      times_best = dplyr::if_else(base::is.na(times_best), 0, times_best),
      level = level
    )
  return(summary)
}

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
#' \item `estimate`: Shows proportion of success estimates user specified normal or empirical confidence intervals.
#' }
#' @param estimator Estimator to plot; Either "AIPW", "Sample" or "Both"; used by `hist` and `estimate`.
#' @param save Logical; Whether or not to save the plot to disk; FALSE by default.
#' @param path String; File directory to save file.
#' @param ... arguments to pass to `ggplot2:geom_*` function (e.g. `color`, `linewidth`, `alpha`, `bins` etc.)
#' @param cdf String; specifies the type of CDF to use when analyzing the estimates.
#' valid cdfs are the `empirical` cdf, the `normal` cdf. Used when type = `estimate`.
#' @inheritParams summary.multiple.mab
#' @example inst/examples/plot.multiple.mab_example.R
#' @export
#' @returns Minimal ggplot object, that can be customized and added to with `+` (To change, scales, labels, legend, theme, etc.)

plot.multiple.mab <- function(x, type, estimator = NULL, cdf = NULL, level = 0.95, save = FALSE, path = NULL, ...) {
  rlang::check_installed("ggplot2")
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
#' @keywords internal

plot_summary <- function(x, ...) {
  rlang::check_installed("ggplot2")
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
#' @keywords internal
plot_hist <- function(x, estimator, ...) {
  rlang::check_installed("ggplot2")
  estimator_arg <- check_estimator(estimator)

  x$estimates |>
    dplyr::filter(estimator %in% estimator_arg) |>
    ggplot2::ggplot(ggplot2::aes(x = mean, y = ggplot2::after_stat(density))) +
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
#' @keywords internal

plot_mult_estimates <- function(x, estimator, cdf, level, ...) {
  rlang::check_installed("ggplot2")
  check_level(level)
  estimator_arg <- check_estimator(estimator)
  if (base::is.null(cdf)) {
    rlang::abort("Invalid Estimator: Valid CDF's are, empirical`, and `normal`")
  }
  cols <- switch(cdf,
    "empirical" = c("upper_empirical", "lower_empirical"),
    "normal" = c("upper_normal", "lower_normal"),
    rlang::abort("Invalid `cdf`: valid cdfs are `normal` or `empirical`")
  )

  summary(x, level = level) |>
    dplyr::filter(estimator %in% estimator_arg) |>
    dplyr::select(!!!rlang::syms(cols), estimator, mab_condition, estimate_avg) |>
    ggplot2::ggplot(ggplot2::aes(x = estimate_avg, y = mab_condition)) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmax = !!rlang::sym(cols[[1]]), xmin = !!rlang::sym(cols[[2]]))) +
    ggplot2::facet_grid(~estimator) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Estimate", y = "Treatment Arm",
      title = "Uncertainy Around Treatment Arm Estimates"
    ) +
    ggplot2::theme(panel.spacing = ggplot2::unit(2, "lines"))
}

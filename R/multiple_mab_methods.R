#' Print Generic For `multiple.mab`
#' @description Custom Print Display for `multiple.mab` objects returned by [multiple_mab_simulation()].
#' Prevents the large list output from being printed to the R console, and provides
#' useful information about the settings for the trials
#' @method print multiple.mab
#' @param x `multiple.mab` class object
#' @param ... further arguments passed to or from other methods
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
#'   ),
#'   times = 5, seeds = sample.int(5)
#' )
#' print(x)
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
#' Summarizes results of of multiple Multi-Arm Bandit Trials. Provides empirically estimated
#' and normally approximated confidence intervals on AIPW estimates for probability of success, and
#' the number of times each arm was the chosen as the best treatment across all simulations.
#' @param object `multiple.mab` object created by [multiple_mab_simulation]
#' @param level Numeric value of length 1; indicates confidence interval Width (i.e 0.90, .95, 0.99).
#' Defaults to 0.95
#' @param ... additional arguments.
#' @method summary multiple.mab
#' @details
#' This summary provides the number of times each treatment arm was
#' selected as the best, chosen by the highest UCB1 or Thompson sampling probabilities
#' from the end of each trial.
#'
#' Additionally it provides the average of the AIPW and sample estimates
#' across all trials, while also providing 2 distinct variances and intervals.
#' One interval is based on the average of the variances, and uses a normal distribution,
#' while an empirical distribution is estimated using the sample of estimates
#' created by the repeated trials. These empirical
#' variances represent the variation in each simulation due to the random state
#'
#' `...` is provided to be compatible with `summary()`, the function
#' does not have any additional arguments.
#'
#' @example inst/examples/summary.multiple.mab_example.R
#' @returns Tibble containing summary information for repeated trials.
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
      SE_avg = sqrt(base::mean(variance, na.rm = TRUE)),
      SE_empirical = sqrt(stats::var(mean)), .groups = "drop",
    ) |>
    dplyr::mutate(
      lower = estimate_avg + stats::qnorm(lower_level) * SE_avg,
      upper = estimate_avg + stats::qnorm(upper_level) * SE_empirical
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
    ) |>
    dplyr::filter(estimator == "AIPW") |>
    dplyr::select(-estimator) |>
    dplyr::rename(
      "average_probability_of_success" = "estimate_avg",
      Treatment_Arm = mab_condition
    )
  return(summary)
}

#' Plot Generic for `multiple.mab` objects
#' @description Uses [ggplot2::ggplot()] to plot the results of multiple
#' Multi-Arm Bandit Trials
#'
#' @method plot multiple.mab
#' @param x `multiple.mab` class object created by [multiple_mab_simulation()]
#' @param type String; Type of plot requested; valid types are:
#' \itemize{
#' \item `summary`: Shows the number of times each arm was selected as the highest chance of being the best.
#' \item `hist`: Shows histograms for each treatment condition's proportion of success across trials.
#' \item `estimate`: Shows proportion of success estimates using specified normal or empirical confidence intervals.
#' }
#' @param save Logical; Whether or not to save the plot to disk; FALSE by default.
#' @param path String; File directory to save file.
#' @param ... arguments to pass to `ggplot2:geom_*` function (e.g. `color`, `linewidth`, `alpha`, `bins` etc.)
#' @param cdf String; specifies the type of CDF to use when analyzing the estimates.
#' valid CDFs are the 'empirical' CDF, the 'normal' CDF. Used when type = `estimate`. The 'normal' CDF uses the fact
#' that the AIPW estimates are asymptotically normal, while the empirical CDF estimates the CDF from the sample
#' of AIPW estimates.
#' @inheritParams summary.multiple.mab
#' @details
#' The plot generic requires \href{https://cran.r-project.org/package=ggplot2}{ggplot2}
#' which is not required by the package, so it must be installed separately.
#'
#' This function provides minimalist plots to quickly view the results of the procedure
#' and has the ability to be customized through the `...`
#' in the call and `+` afterwords. However, all the data necessary is
#' provided in the output of [multiple_mab_simulation()] for extreme
#' customization or professional plots, it is highly recommended
#' to start completely from scratch and not use the generic.
#'
#' @example inst/examples/plot.multiple.mab_example.R
#' @export
#' @returns Minimal ggplot object, that can be customized and added to with `+` (to change, scales, labels, legend, theme, etc.)

plot.multiple.mab <- function(x, type, cdf = NULL, level = 0.95, save = FALSE, path = NULL, ...) {
  rlang::check_installed("ggplot2")
  plot <- switch(type,
    "summary" = plot_summary(x = x, ...),
    "hist" = plot_hist(x = x, ...),
    "estimate" = plot_mult_estimates(
      x = x, cdf = cdf,
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
#' @returns Minimal ggplot object, that can be customized and added to with `+` (to change, scales, labels, legend, theme, etc.)
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
#' @title Plots Distribution of AIPW and Sample estimates over trials
#' @description
#' Plots Distribution of AIPW and Sample estimates over trials for [plot.multiple.mab()]
#' @inheritParams plot.multiple.mab
#' @returns Minimal ggplot object, that can be customized and added to with `+` (to change, scales, labels, legend, theme, etc.)
#' @keywords internal
plot_hist <- function(x, ...) {
  rlang::check_installed("ggplot2")

  x$estimates |>
    ggplot2::ggplot(ggplot2::aes(x = mean, y = ggplot2::after_stat(density))) +
    ggplot2::geom_histogram(...) +
    ggplot2::facet_grid(~mab_condition) +
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
#' Plots AIPW/Sample Estimates for each arm using variance from the repeated trials for [plot.multiple.mab()]
#' @inheritParams plot.multiple.mab
#' @returns Minimal ggplot object, that can be customized and added to with `+` (to change, scales, labels, legend, theme, etc.)
#' @keywords internal

plot_mult_estimates <- function(x, cdf, level, ...) {
  rlang::check_installed("ggplot2")
  check_level(level)
  if (base::is.null(cdf)) {
    rlang::abort("Invalid Estimator: Valid CDF's are, empirical`, and `normal`")
  }
  cols <- switch(cdf,
    "empirical" = c("upper_empirical", "lower_empirical"),
    "normal" = c("upper_normal", "lower_normal"),
    rlang::abort("Invalid `CDF`: valid CDFs are `normal` or `empirical`")
  )

  summary(x, level = level) |>
    dplyr::select(!!!rlang::syms(cols), Treatment_Arm, average_probability_of_success) |>
    ggplot2::ggplot(ggplot2::aes(x = average_probability_of_success, y = Treatment_Arm)) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmax = !!rlang::sym(cols[[1]]), xmin = !!rlang::sym(cols[[2]])), ...) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Probability of Succcess (AIPW)", y = "Treatment Arm",
      title = "Uncertainy Around Treatment Arm Estimates"
    )
}

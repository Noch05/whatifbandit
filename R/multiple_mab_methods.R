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
  base::cat("Trials Conducted:     ", settings$trials, "trials\n")
  base::cat("Keep Final Data:      ", settings$keep_data, "\n")
  base::cat("----------------------------------------------------- \n")
}
#------------------------------------------------------------------------------
#' Summary Generic For "multiple.mab" Class
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
    dplyr::summarize(mean = base::mean(value), standard_dev = stats::sd(value))

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
      estimate_avg = base::mean(mean, na.rm = TRUE),
      SE_avg = sqrt(base::mean(variance, na.rm = TRUE)),
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
      times_best = dplyr::if_else(base::is.na(times_best), 0, times_best),
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
#' @param save Logical; Whether or not to save the plot to disk; FALSE by default.
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
  plot <- switch(
    type,
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
  data <- switch(
    quantity,
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
  plot_labels <- switch(
    quantity,
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
  if (base::is.null(cdf)) {
    rlang::abort("Invalid Estimator: Valid CDF's are, empirical`, and `normal`")
  }
  cols <- switch(
    cdf,
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

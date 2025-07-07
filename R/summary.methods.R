#' Summary Generic for "mab" class
#' @description
#' Summarizes the Results of a Single Multi-Arm Bandit Trial.
#' @param object "mab" class object created by [single_mab_simulation()].
#' @param level Confidence Interval Width (i.e 0.90, .95, 0.99)
#' @param ... additional arguments.
#' @method summary mab
#' @export
#' @returns data.frame containg each treatment, the final Thompson/UCB1 Statistic,
#' the AIPW estimate and Normal CI based on user supplied level.
summary.mab <- function(object, level = 0.95, ...) {
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
      Lower_Bound = mean - normalq * sqrt(variance),
      Upper_Bound = mean + normalq * sqrt(variance)
    ) |>
    dplyr::select(-variance, -estimator) |>
    dplyr::rename("AIPW" = "mean") |>
    dplyr::mutate(
      level = level,
      periods = periods
    )
}
#------------------------------------------------------------------------------
#' Summary Generic for "multiple.mab" class
#' @description
#' Summarizes results of of multiple Multi-Arm Bandit Trials
#' @param object `multiple.mab` object created by [multiple_mab_simulation]
#' @param level Confidence Interval Width (i.e 0.90, .95, 0.99)
#' @param ... additional arguments.
#' @method summary multiple.mab
#' @export

summary.multiple.mab <- function(object, level = 0.95, ...) {
  lower_level <- (1 - level) / 2
  upper_level <- 1 - lower_level

  quantiles <- object$estimates |>
    dplyr::filter(estimator == "AIPW") |>
    dplyr::group_by(mab_condition) |>
    dplyr::summarize(
      lower = stats::quantile(mean, lower_level),
      upper = stats::quantile(mean, upper_level)
    )

  estimate <- object$estimates |>
    dplyr::filter(estimator == "AIPW") |>
    dplyr::group_by(mab_condition) |>
    dplyr::summarize(
      avg_estimate = base::mean(mean, na.rm = TRUE),
      variance_avg = base::mean(variance, na.rm = TRUE),
      variance_resample = stats::var(mean), .groups = "drop",
    ) |>
    dplyr::mutate(
      lower = avg_estimate - qnorm(upper_level) * variance_avg,
      upper = avg_estimate + qnorm(lower_level) * variance_avg
    ) |>
    left_join(quantiles, by = "mab_condition", suffix = c("_normal", "_empirical"))

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

  summary <- dplyr::left_join(estimate, bandits, by = "mab_condition") |>
    dplyr::rename(times_best = "n") |>
    dplyr::mutate(times_best = dplyr::if_else(base::is.na(times_best), 0, times_best))
  return(summary)
}

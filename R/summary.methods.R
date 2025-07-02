#' Summary Generic for "mab" class
#' @description
#' Summarizes the Results of a Single Multi-Arm Bandit Trial.
#' @param x "mab" class object created by [single_mab_simulation()].
#' @param level Confidence Interval Width (i.e 0.90, .95, 0.99)
#' @param ... additional arguments.
#' @method summary mab
#' @export
#' @returns data.frame containg each treatment, the final Thompson/UCB1 Statistic,
#' the AIPW estimate and Normal CI based on user supplied level.
summary.mab <- function(x, level = 0.95, ...) {
  periods <- base::max(x$bandits$period_number)
  col2 <- switch(x$settings$algorithm,
    "UCB1" = "UCB1_Statistic",
    "Thompson" = "Probability_Of_Best_Arm"
  )
  estimates <- x$estimates |>
    dplyr::filter(estimator == "AIPW")

  normalq <- base::abs(stats::qnorm((1 - level) / 2))

  x$bandits[periods, ] |>
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

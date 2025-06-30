#' Calculate Adaptive AIPW Estimates
#' @name adaptive_aipw
#'
#' @description Takes the average of the individual AIPW scores created by [get_iaipw()] for each period,
#' and assigns each estimate an adaptive weight based on a constant allocation rate across periods defined by
#' \href{https://www.pnas.org/doi/pdf/10.1073/pnas.2014602118}{Hadad et. al (2021)} to calculate a final
#' estimate for each treatment condition.
#'
#' @param periods Numeric; number of treatment waves.
#' @inheritParams get_adaptive_aipw
#'
#'
#' @returns A data.frame containing:
#' \item{mean}{Adaptive AIPW estimate for each treatment}
#' \item{variance}{Variance of each estimate}
#'
#' @seealso
#' * [get_iaipw()]
#' * [get_adaptive_aipw()]
#' * [single_mab_simulation()]
#' @export
#-------------------------------------------------------------------------------
## Adaptive AIPW Weights from Hadad et. al (2021) Using Constant Allocation Rate
adaptive_aipw <- function(mab, conditions, periods, algorithm, verbose) {
  verbose_log(verbose, "Aggregating AIPW Estimates")

  data <- mab[["final_data"]]

  estimates <- base::vector(mode = "list", length = periods)
  probs <- mab[["assignment_probs"]]

  for (i in base::seq_len(length(conditions))) {
    results <- data |>
      dplyr::group_by(period_number) |>
      dplyr::summarize(avg = base::mean(!!rlang::sym(base::paste0("aipw_", conditions[i])), na.rm = TRUE)) |>
      dplyr::mutate(time_weights = purrr::map_dbl(period_number, ~ base::sqrt(
        base::as.numeric(probs[.x, conditions[i]]) / periods
      )))

    estimate <- results |>
      dplyr::summarize(estimate = base::sum(avg * time_weights, na.rm = TRUE) / base::sum(time_weights, na.rm = TRUE)) |>
      dplyr::pull()

    variance <- results |>
      dplyr::mutate(time_weights_sq = time_weights^2) |>
      dplyr::summarize(variance = base::sum(time_weights_sq * (avg - estimate)^2) / (base::sum(time_weights))^2) |>
      dplyr::pull()

    estimates[[i]] <- tibble::tibble(
      mean = estimate,
      variance = variance,
      mab_condition = conditions[i]
    )
  }

  sample <- data |>
    dplyr::group_by(mab_condition) |>
    dplyr::summarize(
      mean = base::mean(mab_success, na.rm = TRUE),
      variance = stats::var(mab_success, na.rm = TRUE)
    ) |>
    dplyr::mutate(estimator = "Sample")


  returns <- dplyr::bind_rows(estimates) |>
    dplyr::mutate(estimator = "AIPW") |>
    base::rbind(sample)
  return(returns)
}

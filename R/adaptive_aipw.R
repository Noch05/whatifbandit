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
adaptive_aipw <- function(mab, conditions, periods, algorithm) {
  conditions <- base::sort(conditions)

  data <- mab[[1]]

  estimates <- base::vector(mode = "list", length = periods)

  if (algorithm == "Thompson") {
    bandits <- mab[[2]]
  } else if (algorithm == "UCB1") {
    selected_arms <- mab[[2]] |>
      tidyr::pivot_longer(cols = -period, names_to = "mab_condition", values_to = "ucb") |>
      dplyr::group_by(period) |>
      slice_max(order_by = dplyr::desc(ucb), with_ties = FALSE)

    bandits <- lapply(base::seq_len(periods), function(i) {
      if (i == 1) {
        bandits <- rlang::set_names(rep(
          1 / base::length(conditions),
          base::length(conditions)
        ), conditions)
        return(bandits)
      } else {
        bandits <- base::ifelse(selected_arms$mab_condition[i] == conditions, 1, 0)

        base::names(bandits) <- conditions
        return(bandits)
      }
    }) |>
      dplyr::bind_rows(.id = "period")
  } else {
    base::stop("Please Specify UCB1 or Thompson for algorithm")
  }


  for (i in base::seq_len(length(conditions))) {
    results <- data |>
      dplyr::group_by(period_number) |>
      dplyr::summarize(avg = base::mean(!!rlang::sym(base::paste0("aipw_", conditions[i])), na.rm = TRUE)) |>
      dplyr::mutate(time_weights = purrr::map_dbl(period_number, ~ base::sqrt(
        base::as.numeric(bandits[.x, conditions[i]]) / periods
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

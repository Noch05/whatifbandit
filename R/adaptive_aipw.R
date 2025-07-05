#' Calculate Adaptive AIPW Estimates
#' @name adaptive_aipw
#'
#' @description Takes the average of the individual AIPW scores created by [get_iaipw()] for each period,
#' and assigns each estimate an adaptive weight based on a constant allocation rate across periods defined by
#' \href{https://www.pnas.org/doi/pdf/10.1073/pnas.2014602118}{Hadad et. al (2021)} to calculate a final
#' estimate for each treatment condition.
#'
#' @inheritParams get_adaptive_aipw
#' @inheritParams single_mab_simulation
#'
#'
#'
#'
#' @seealso
#' * [get_iaipw()]
#' * [get_adaptive_aipw()]
#' * [mab_simulation()]
#-------------------------------------------------------------------------------
## Adaptive AIPW Weights from Hadad et. al (2021) Using Constant Allocation Rate
adaptive_aipw <- function(data, assignment_probs, conditions, periods, algorithm, verbose) {
  verbose_log(verbose, "Aggregating AIPW Estimates")
  base::UseMethod("adaptive_aipw")
}
#-------------------------------------------------------------------------------
#' @title Adaptive AIPW Estimates for Tibbles
#' @method adaptive_aipw tbl_df
#' @inheritParams adaptive_aipw
adaptive_aipw.tbl_df <- function(data, assignment_probs, conditions, periods, algorithm, verbose) {
  estimates <- base::vector(mode = "list", length = periods)

  for (i in base::seq_len(length(conditions))) {
    results <- data |>
      dplyr::group_by(period_number) |>
      dplyr::summarize(avg = base::mean(!!rlang::sym(base::paste0("aipw_", conditions[i])), na.rm = TRUE)) |>
      dplyr::mutate(time_weights = purrr::map_dbl(period_number, ~ base::sqrt(
        base::as.numeric(assignment_probs[.x, conditions[i]]) / periods
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
#-------------------------------------------------------------------------------
#' @title Adaptive AIPW Estimates for data.frames
#' @method adaptive_aipw data.frame
#' @inheritParams adaptive_aipw
adaptive_aipw.data.frame <- function(data, assignment_probs, conditions, periods, algorithm, verbose) {
  return(
    adaptive_aipw.tbl_df(
      data = tibble::as_tibble(data),
      assignment_probs = tibble::as_tibble(assignment_probs),
      conditons = conditions,
      periods = periods,
      algorithm = algorithm,
      verbose = verbose
    )
  )
}

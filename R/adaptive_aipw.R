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
#' @keywords internal
#-------------------------------------------------------------------------------
## Adaptive AIPW Weights from Hadad et. al (2021) Using Constant Allocation Rate
adaptive_aipw <- function(data, assignment_probs, conditions, periods, verbose) {
  verbose_log(verbose, "Aggregating AIPW Estimates")

  base::UseMethod("adaptive_aipw", data)
}
#-------------------------------------------------------------------------------
#' @title Adaptive AIPW Estimates for data.frames
#' @method adaptive_aipw data.frame
#' @inheritParams adaptive_aipw
#' @noRd
#'
adaptive_aipw.data.frame <- function(data, assignment_probs, conditions, periods, verbose) {
  estimates <- purrr::map(
    conditions, ~ {
      results <- data |>
        dplyr::group_by(period_number) |>
        dplyr::summarize(
          avg = base::mean(!!rlang::sym(base::paste0("aipw_", .x)), na.rm = TRUE),
          assign_prob = base::unique(!!rlang::sym(base::paste0(.x, "_assign_prob")))
        ) |>
        dplyr::mutate(time_weights = (assign_prob / periods))


      estimate <- (base::sum(results$avg * results$time_weights, na.rm = TRUE)) /
        (base::sum(results$time_weights, na.rm = TRUE))

      variance <- base::sum((results$time_weights^2) * (results$avg - estimate)^2) /
        (base::sum(results$time_weights)^2)

      return(tibble::tibble(
        mean = estimate,
        variance = variance,
        mab_condition = .x
      ))
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(estimator = "AIPW")

  sample <- data |>
    dplyr::group_by(mab_condition) |>
    dplyr::summarize(
      mean = base::mean(mab_success, na.rm = TRUE),
      variance = ((mean * (1 - mean) / dplyr::n()))
    ) |>
    dplyr::mutate(estimator = "Sample")

  returns <- dplyr::bind_rows(estimates, sample)


  return(returns)
}
#-------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#' @title Adaptive AIPW Estimates for data.tables
#' @method adaptive_aipw data.table
#' @inheritParams adaptive_aipw
#' @noRd
adaptive_aipw.data.table <- function(data, assignment_probs, conditions,
                                     periods, verbose) {
  estimates <- purrr::map(
    conditions, ~ {
      results <- data[, .(
        avg = base::mean(base::get(base::paste0("aipw_", .x)), na.rm = TRUE),
        assign_prob = base::unique(base::get(base::paste0(.x, "_assign_prob")))
      ),
      by = period_number
      ]
      results[, time_weights := assign_prob / periods]

      estimate <- (base::sum(results$avg * results$time_weights, na.rm = TRUE)) /
        (base::sum(results$time_weights, na.rm = TRUE))

      variance <- base::sum((results$time_weights^2) * (results$avg - estimate)^2) /
        (base::sum(results$time_weights)^2)

      return(data.table::data.table(
        mean = estimate,
        variance = variance,
        mab_condition = .x
      ))
    }
  )
  estimates <- data.table::rbindlist(estimates)
  estimates[, estimator := "AIPW"]



  sample <- data[, .(
    mean = base::mean(mab_success, na.rm = TRUE),
    variance = ((mean(mab_success) * (1 - mean(mab_success))) / .N)
  ),
  by = mab_condition
  ]

  sample[, estimator := "Sample"]
  returns <- data.table::rbindlist(list(estimates, sample),
    use.names = TRUE
  )

  return(returns)
}

#' Calculate Individual AIPW For Each Treatment Condition
#' @name get_iaipw
#' @description Calculates the individual Augmented Inverse Probability Weighted Estimate (AIPW) of treatment
#' success for each treatment condition provided. This method scales the estimated probabilities of success by
#' the probability of being assigned the treatment, and weighted  by a the conditional expectation of success
#' from prior periods of an adaptive trial. The condition expectation function is a grouped mean by
#' treatment arm.
#'
#' @inheritParams single_mab_simulation
#' @param periods Numeric value of length 1; number of total periods in the simulation
#' @param assignment_probs tibble/data.table containing the probabilities of being
#' assigned each treatment at a given period
#'
#' @returns A tibble/data.frame, containing the data used in the Multi-Arm-Bandit, with
#' new columns pertaining to the individual AIPW estimate for each person and condition, and
#' probability of assignment for each treatment at each period.
#'
#' @details
#' The specification for the Individual AIPW estimates can be found
#' in \href{https://www.pnas.org/doi/full/10.1073/pnas.2014602118}{Hadad et al. (2021)}. The
#' formulas in equation 5, formed the basis for this function's calculations. Here
#' the regression adjustment used is the grouped mean of success by treatment, up until
#' the current period of estimation (so at period 5, the grouped mean was calculated
#' using data from periods 1 through 4).
#'
#' The AIPW estimator makes corrections for the non-random method of assigning treatment,
#' and is both unbiased and asymptotically normal, so can be used fo statistical
#' inference, while the sample mean in this case cannot.
#' @references
#' Hadad, Vitor, David A. Hirshberg, Ruohan Zhan, Stefan Wager, and Susan Athey. 2021.
#' “Confidence Intervals for Policy Evaluation in Adaptive Experiments.” \emph{Proceedings of the National Academy of Sciences of the United States of America} 118
#' (15): e2014602118. \doi{10.1073/pnas.2014602118}.
#'
#' @keywords internal
get_iaipw <- function(data, assignment_probs, periods, conditions, verbose) {
  verbose_log(verbose, "Computing Individual AIPW Estimates")
  base::UseMethod("get_iaipw", data)
}
#-------------------------------------------------------------------------------
#' @method get_iaipw data.frame
#' @title
#' [get_iaipw()] for data.frames
#' @inheritParams get_iaipw
#' @noRd

get_iaipw.data.frame <- function(data, assignment_probs, periods, conditions, verbose) {
  new_cols <- paste0("aipw_", conditions)
  data[new_cols] <- NA_real_


  prior_data <- data |>
    dplyr::group_by(period_number, mab_condition) |>
    dplyr::summarize(
      successes = sum(mab_success, na.rm = TRUE),
      trials = dplyr::n(),
      .groups = "drop"
    )
  names(assignment_probs) <- c("period_number", paste0(names(assignment_probs)[-1], "_assign_prob"))

  data <- base::expand.grid(
    period_number = base::seq_len(periods),
    mab_condition = conditions
  ) |>
    dplyr::left_join(prior_data, by = c("period_number", "mab_condition")) |>
    dplyr::mutate(dplyr::across(c(successes, trials), ~ tidyr::replace_na(.x, 0))) |>
    dplyr::arrange(mab_condition, period_number) |>
    dplyr::group_by(mab_condition) |>
    dplyr::mutate(
      cumulative_successes = dplyr::lag(base::cumsum(successes), default = 0),
      cumulative_trials = dplyr::lag(base::cumsum(trials), default = 0),
      prior_period_success_rate = dplyr::if_else(
        cumulative_trials > 0, cumulative_successes / cumulative_trials, 0
      )
    ) |>
    dplyr::select(period_number, mab_condition, prior_period_success_rate) |>
    tidyr::pivot_wider(
      names_from = mab_condition, values_from = "prior_period_success_rate",
      names_prefix = "prior_rate_"
    ) |>
    dplyr::right_join(data, by = "period_number") |>
    dplyr::select(!!!rlang::syms(names(data)), tidyr::starts_with("prior_rate_")) |>
    dplyr::left_join(assignment_probs, by = "period_number")

  for (i in base::seq_along(conditions)) {
    verbose_log(verbose, paste0("Condition: ", conditions[[i]]))

    probability <- data[[paste0(conditions[[i]], "_assign_prob")]]
    mhat <- data[[paste0("prior_rate_", conditions[[i]])]]

    data[[paste0("aipw_", conditions[[i]])]] <- base::ifelse(
      data$mab_condition == conditions[[i]],
      (data$mab_success / probability) + (1 - (1 / probability)) * mhat,
      mhat
    )
  }


  check <- data |>
    dplyr::summarize(dplyr::across(dplyr::starts_with("aipw_"), ~ base::sum(base::is.na(.x)))) |>
    base::sum()

  if (check != 0) {
    base::warning(paste0(check, " Individual AIPW Scores are NA"))
  }
  return(data)
}
# ------------------------------------------------------------------------------
#' @method get_iaipw data.table
#' @title [get_iaipw()] for data.tables
#' @inheritParams get_iaipw
#' @noRd

get_iaipw.data.table <- function(data, assignment_probs, periods, conditions, verbose) {
  new_cols <- paste0("aipw_", conditions)
  data[, (new_cols) := NA_real_]

  prior_data <- data[, .(
    successes = base::sum(mab_success),
    trials = .N
  ), by = c("mab_condition", "period_number")]
  data.table::setkey(prior_data, period_number)

  full_grid <- data.table::CJ(
    period_number = base::seq_len(periods),
    mab_condition = conditions
  )
  full_grid <- merge(full_grid, prior_data,
    by = c("period_number", "mab_condition"),
    suffixes = c("", ""), all = TRUE
  )
  full_grid[is.na(full_grid)] <- 0

  data.table::setorder(full_grid, mab_condition, period_number)
  full_grid[, `:=`(
    cumulative_successes = data.table::shift(base::cumsum(successes),
      n = 1L, type = "lag", fill = 0
    ),
    cumulative_trials = data.table::shift(base::cumsum(trials),
      n = 1L, type = "lag", fill = 0
    )
  ), by = c("mab_condition")]
  full_grid[, prior_period_success_rate := data.table::fifelse(
    cumulative_trials > 0, cumulative_successes / cumulative_trials, 0
  )]

  full_grid <- data.table::dcast(
    data = full_grid[, .(period_number, mab_condition, prior_period_success_rate)],
    formula = period_number ~ mab_condition, value.var = "prior_period_success_rate"
  )
  data.table::setnames(full_grid, c("period_number", paste0("prior_rate_", names(full_grid)[-1])))
  data.table::setnames(assignment_probs, c("period_number", paste0(names(assignment_probs)[-1], "_assign_prob")))

  data <- merge(data, full_grid, all = TRUE, by = "period_number")
  data <- merge(data, assignment_probs,
    by = "period_number",
    all = TRUE, suffixes = c("", "_assign_prob")
  )


  for (i in base::seq_along(conditions)) {
    verbose_log(verbose, paste0("Condition: ", conditions[[i]]))
    probability <- paste0(conditions[[i]], "_assign_prob")
    mhat <- paste0("prior_rate_", conditions[[i]])


    data[, (paste0("aipw_", conditions[[i]])) := data.table::fifelse(
      mab_condition == conditions[[i]],
      (mab_success / get(probability)) + (1 - 1 / get(probability)) * get(mhat),
      get(mhat),
    )]
  }

  check <- base::sum(base::unlist(data[, lapply(.SD, \(x) base::sum(base::is.na(x))), .SDcols = new_cols]))


  if (check != 0) {
    base::warning(paste0(check, " Individual AIPW Scores are NA"))
  }

  return(invisible(data))
}


#-------------------------------------------------------------------------------
#' Calculate Adaptive AIPW Estimates
#' @name adaptive_aipw
#'
#' @description Takes the average of the individual AIPW scores created by [get_iaipw()] for each period,
#' and assigns each estimate an adaptive weight based on a constant allocation rate of variance across periods defined by
#' \href{https://www.pnas.org/doi/full/10.1073/pnas.2014602118}{Hadad et. al (2021)} to calculate a final
#' AIPW estimate and variance for each treatment condition. Sample means and variances are also provided
#' for comparison.
#'
#' @inheritParams get_iaipw
#' @inheritParams single_mab_simulation
#' @returns tibble/data.table containing the AIPW estimate of treatment success, AIPW variance,
#' sample proportion of successful treatments (sample mean), and sample mean variance.
#' @details
#' The formulas for the calculations in this function can be found in
#' \href{https://www.pnas.org/doi/full/10.1073/pnas.2014602118}{Hadad et al. (2021)} at
#' equation 5 (estimate), equation 11 (variance), equation 15 (allocation rate).
#'
#' The formulas specified assume that each period is 1 observation but in the cases
#' for this simulation where periods contain multiple observations the individual estimates
#' from each period are averaged before being used in the final calculations.
#'
#' The AIPW estimator makes corrections for the non-random method of assigning treatment,
#' and is both unbiased and asymptotically normal, so can be used fo statistical
#' inference, while the sample mean which is still provided for comparison, cannot be.
#' @references
#' Hadad, Vitor, David A. Hirshberg, Ruohan Zhan, Stefan Wager, and Susan Athey. 2021.
#' “Confidence Intervals for Policy Evaluation in Adaptive Experiments.” \emph{Proceedings of the National Academy of Sciences of the United States of America} 118
#' (15): e2014602118. \doi{10.1073/pnas.2014602118}.
#' @keywords internal
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

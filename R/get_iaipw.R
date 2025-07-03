#' Calculate Individual AIPW For Each Treatment Condition
#' @name get_iaipw
#' @description Calculates the individual Augmented Inverse Probability Weighted Estimate (AIPW) of treatment
#' success for each treatment condition provided.
#'
#'
#' @inheritParams get_adaptive_aipw
#' @inheritParams single_mab_simulation
#' @param periods Numeric; number of treatment waves.
#'
#'
#' @returns A data frame containing the data used in the MAB trial
#' with new columns corresponding to the individual AIPW estimate for each treatment condition, and
#' the probability of being assigned a given treatment condition.
#'
#' @seealso
#' * [run_mab_trial()]
#' * [get_adaptive_aipw()]
#' * [single_mab_simulation()]
#'
get_iaipw <- function(mab, periods, algorithm, conditions, verbose) {
  verbose_log(verbose, "Computing Individual AIPW Estimates")

  data_class <- class(mab[[1]])
  if ("data.table" %in% data_class) {
    return(get_iaipw.data.table(
      mab = mab,
      periods = periods,
      algorithm = algorithm,
      conditions = conditions,
      verbose = verbose
    ))
  } else {
    return(get_iaipw.tbl_df(
      mab = mab,
      periods = periods,
      algorithm = algorithm,
      conditions = conditions,
      verbose = verbose
    ))
  }
}
#-------------------------------------------------------------------------------

#'
#' @method get_iaipw tbl_df
#' @title
#' [get_iaipw()] for tibbles
#' @inheritParams get_iaipw


get_iaipw.tbl_df <- function(mab, periods, algorithm, conditions, verbose) {
  data <- mab[["final_data"]]

  new_cols <- paste0("aipw_", conditions)
  data[new_cols] <- NA_real_


  prior_data <- data |>
    dplyr::group_by(period_number, mab_condition) |>
    dplyr::summarize(
      successes = sum(mab_success, na.rm = TRUE),
      trials = dplyr::n(),
      .groups = "drop"
    )
  probs <- mab[["assignment_probs"]]
  names(probs) <- c("period_number", paste0(names(probs)[-1], "_assign_prob"))

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
    dplyr::left_join(probs, by = "period_number")

  for (condition in conditions) {
    verbose_log(verbose, paste0("Condition: ", condition))

    probability <- data[[paste0(condition, "_assign_prob")]]
    mhat <- data[[paste0("prior_rate_", condition)]]

    data[[paste0("aipw_", condition)]] <- base::ifelse(
      data$mab_condition == condition,
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
#'
get_iaipw.data.table <- function(mab, periods, algorithm, conditions, verbose) {
  data <- mab[["final_data"]]
  new_cols <- paste0("aipw_", conditions)
  data[, (new_cols) := NA_real_]

  prior_data <- data[, .(
    successes = base::sum(mab_success),
    trials = .N
  ), by = .(period_number, mab_condition)]


  probs <- mab[["assignment_probs"]]
  names(probs) <- c("period_number", paste0(names(probs)[-1], "_assign_prob"))

  grid <- data.table::CJ(
    period_number = base::seq_len(periods),
    mab_condition = conditions
  )

  prior_data <- merge(grid, prior_data, by = c("mab_condition", "period_number"), all.x = TRUE)
  prior_data[base::is.na(successes), successes := 0][base::is.na(trials), trials := 0]
  data.table::setorder(prior_data, mab_condition, period_number)

  prior_data[, `:=`(
    cumulative_successes = data.table::shift(base::cumsum(successes),
      fill = 0,
      type = "lag", n = 1L
    ),
    cumulative_trials = data.table::shift(base::cumsum(trials),
      fill = 0, type = "lag",
      n = 1L
    )
  ), by = mab_condition]

  prior_data[, prior_period_success_rate := data.table::fifelse(
    cumulative_trials > 0, cumulative_successes / cumulative_trials, 0
  )][
    ,
    .(
      period_number,
      mab_condition,
      prior_period_success_rate
    )
  ]

  prior_data <- data.table::dcast(
    prior_data, period_number ~ mab_condition,
    value.var = "prior_period_success_rate",
  )

  data.table::setnames(prior_data,
    old = base::names(prior_data),
    new = c(
      "period_number",
      base::paste0(
        "prior_rate_",
        base::names(prior_data)[base::names(prior_data) != "period_number"]
      )
    )
  )
  data <- merge(data, prior_data, by = "period_number", all.x = TRUE)
  data <- merge(data, probs, by = "period_number", all.x = TRUE)

  for (condition in conditions) {
    verbose_log(verbose, paste0("Condition: ", condition))

    probability <- base::paste0(condition, "_assign_prob")
    mhat <- base::paste0("prior_rate_", condition)
    aipw <- base::paste0(paste0("aipw_", condition))
    data[, (aipw) := data.table::fifelse(
      mab_condition == condition,
      (mab_success / base::get(probability) + (1 - (1 / base::get(probability))) * base::get(mhat)),
      base::get(mhat)
    )]
  }


  check <- data[, base::lapply(.SD, \(x) base::sum(base::is.na(x))), .SDcols = data.table::patterns("^aipw_")]
  totalNA <- base::sum(base::unlist(check))

  if (totalNA != 0) {
    base::warning(paste0(check, " Individual AIPW Scores are NA"))
  }
  return(data)
}

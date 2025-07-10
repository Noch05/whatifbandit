#' @name end_mab_trial
#' @title Ends Multi-Arm Bandit Trial
#' @description Condenses output from [run_mab_trial()] into
#' manageable structure.
#' @param data finalized data from [run_mab_trial()].
#' @param bandits Finalized bandits list from [run_mab_trial()].
#' @param periods Numeric scalar; total number of periods in Multi-Arm-Bandit trial.
#' @inheritParams single_mab_simulation
#' @return  A named list containing:
#' \itemize{
#' \item `final_data`: Processed data with new treatment assignments and imputed outcomes labelled with "mab_" prefix.
#' \item `bandits`: Thompson Probability or UCB1 statistic for each treatment arm at each period of the simulation.
#' \item `assignment_probs`: Assignment probabilities for each treatment arm at each period of the simulation.
#' }
#' @seealso
#'* [run_mab_trial()]

end_mab_trial <- function(data, bandits, algorithm, periods, conditions) {
  data <- check_dt(data, tibble::as.tibble)
  base::UseMethod("end_mab_trial", data)
}


#-------------------------------------------------------------------------------
#
#' @method end_mab_trial tbl_df
#' @inheritParams end_mab_trial
#' @title [end_mab_trial()] for tibbles

end_mab_trial.tbl_df <- function(data, bandits, algorithm, periods, conditions) {
  final_summary <- data |>
    dplyr::group_by(mab_condition) |>
    dplyr::summarize(
      successes = base::sum(mab_success, na.rm = TRUE),
      success_rate = base::mean(mab_success, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::ungroup()

  final_bandit <- get_bandit(
    past_results = final_summary,
    algorithm = algorithm,
    conditions = conditions,
    current_period = (periods + 1),
    control_augment = 0
  )


  bandits$bandit_stat[[(periods + 1)]] <- final_bandit[[1]]

  bandit_stats <- switch(algorithm,
    "Thompson" = {
      dplyr::bind_rows(bandits$bandit_stat, .id = "period_number") |>
        dplyr::mutate(
          period_number = base::as.numeric(period_number),
          dplyr::across(-period_number, ~ dplyr::lead(., n = 1L, default = NA))
        ) |>
        dplyr::slice(base::seq_len(periods))
    },
    "UCB1" = {
      dplyr::bind_rows(bandits$bandit_stat, .id = "period_number") |>
        dplyr::select(ucb, mab_condition, period_number) |>
        tidyr::pivot_wider(values_from = "ucb", names_from = c("mab_condition")) |>
        dplyr::mutate(
          period_number = base::as.numeric(period_number),
          dplyr::across(-period_number, ~ dplyr::lead(., n = 1L, default = NA))
        ) |>
        dplyr::slice(base::seq_len(periods))
    },
    rlang::abort("Invalid Algorithm: valid algorithsm are `Thompson`, and `UCB1`")
  )


  assignment_probs <- dplyr::bind_rows(bandits$assignment_prob, .id = "period_number") |>
    dplyr::mutate(period_number = base::as.numeric(period_number))

  return(list(
    final_data = data,
    bandits = bandit_stats,
    assignment_probs = assignment_probs
  ))
}
#-------------------------------------------------------------------------------
#' @method end_mab_trial data.frame
#' @inheritParams end_mab_trial
#' @title [end_mab_trial()] for data.frames
end_mab_trial.data.frame <- function(data, bandits, algorithm, periods, conditions) {
  return(
    end_mab_trial.tbl_df(
      data = tibble::as_tibble(data),
      bandits = bandits,
      algorithm = algorithm,
      periods = periods,
      conditions = conditions
    )
  )
}
#-------------------------------------------------------------------------------
#' @method end_mab_trial data.table
#' @inheritParams end_mab_trial
#' @title [end_mab_trial()] for data.tables
end_mab_trial.data.table <- function(data, bandits, algorithm, periods, conditions) {
  final_summary <- data[, .(
    successes = base::sum(mab_success, na.rm = TRUE),
    success_rate = base::mean(mab_success, na.rm = TRUE),
    n = .N
  ), by = mab_condition]

  final_bandit <- get_bandit(
    past_results = final_summary,
    algorithm = algorithm,
    conditions = conditions,
    current_period = (periods + 1),
    control_augment = 0
  )

  bandits$bandit_stat[[(periods + 1)]] <- final_bandit[[1]]

  bandit_stats <- switch(algorithm,
    "Thompson" = {
      x <- data.table::as.data.table(dplyr::bind_rows(bandits$bandit_stat,
        .id = "period_number"
      ))
      x[, period_number := base::as.numeric(period_number)]

      x[, (conditions) := lapply(.SD, function(col) {
        data.table::shift(col,
          n = 1L,
          type = "lead",
          fill = NA
        )
      }),
      .SDcols = conditions
      ]
      x[base::seq_len(periods), ]
    },
    "UCB1" = {
      x <- data.table::rbindlist(bandits$bandit_stat,
        use.names = TRUE,
        fill = TRUE,
        idcol = "period_number"
      )
      x <- data.table::dcast(
        data = x[, .(ucb, mab_condition, period_number)],
        formula = period_number ~ mab_condition, value.var = "ucb"
      )

      x[, period_number := base::as.numeric(period_number)]

      x[, (conditions) := base::lapply(.SD, function(col) {
        data.table::shift(col,
          n = 1L,
          type = "lead",
          fill = NA
        )
      }),
      .SDcols = conditions
      ]
      x[base::seq_len(periods), ]
    },
    rlang::abort("Invalid Algorithm: valid algorithsm are `Thompson`, and `UCB1`")
  )

  assignment_probs <- data.table::as.data.table(dplyr::bind_rows(bandits$assignment_prob, .id = "period_number"))
  assignment_probs[, period_number := base::as.numeric(period_number)]

  return(list(
    final_data = data,
    bandits = bandit_stats,
    assignment_probs = assignment_probs
  ))
}

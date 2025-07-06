#' @name end_mab_trial
#' @title Ends Multi-Arm Bandit Trial
#' @description Condenses output from [run_mab_trial()] into
#' manageable structure.
#' @param data finalized data from [run_mab_trial()]
#' @param bandits Finalized bandits list from [run_mab_trial()]
#'
#' @seealso
#'* [run_mab_trial()]

end_mab_trial <- function(data, bandits) {
  base::UseMethod("end_mab_trial")
}


#-------------------------------------------------------------------------------
#
#' @method end_mab_trial tbl_df
#' @inheritParams end_mab_trial
#' @title [end_mab_trial()] for tibbles

end_mab_trial.tbl_df <- function(data, bandits) {
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
    control_augment = control_augment
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


  assignment_probs <- dplyr::bind_rows(bandits[["assignment_prob"]], .id = "period_number") |>
    dplyr::mutate(period_number = base::as.numeric(period_number))

  return(list(
    data = data,
    bandits = bandit_stats,
    assignmnet_probs = assignment_probs
  ))
}
#-------------------------------------------------------------------------------
#' @method end_mab_trial data.frame
#' @inheritParams end_mab_trial
#' @title [end_mab_trial()] for data.frames
end_mab_trial.data.frame <- function(data, bandits) {
  return(
    end_mab_trial.tbl_df(
      data = tibble::as_tibble(data),
      bandits = bandits
    )
  )
}
#-------------------------------------------------------------------------------

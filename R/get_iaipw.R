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
#' with new columns corresponding to the individual AIPW estimate for each treatment condition.
#'
#' @seealso
#' * [run_mab_trial()]
#' * [get_adaptive_aipw()]
#' * [single_mab_simulation()]
#' @export


get_iaipw <- function(mab, periods, algorithm, conditions, id_col, verbose) {
  data <- mab[[1]]
  bandits <- mab[[2]]
  conditions <- base::sort(conditions)

  for (i in 1:base::length(conditions)) {
    data[[base::paste0("aipw_", conditions[i])]] <- NA_real_
  }

  for (i in base::seq_len(periods)) {
    if (verbose) {
      base::cat(base::paste0("Period_", i, "\n"))
    }
    prior <- base::seq_len(i - 1)

    prior_data <- data |>
      dplyr::filter(period_number %in% prior) |>
      dplyr::group_by(mab_condition) |>
      dplyr::summarize(success_rate = base::mean(mab_success, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(mab_condition)

    if (nrow(prior_data) != length(conditions)) {
      add_cond <- tibble::tibble(
        mab_condition = base::setdiff(conditions, prior_data$mab_condition),
        success_rate = 0
      )
      prior_data <- base::rbind(prior_data, add_cond)
    }

    if (algorithm == "Thompson") {
      bandit <- bandits[i, ]
    } else if (algorithm == "UCB1") {
      if (i == 1) {
        bandit <- rlang::set_names(base::rep(1 / base::length(conditions), base::length(conditions)), base::sort(conditions))
      } else {
        bandit <- rlang::set_names(base::rep(1, base::length(conditions)), base::sort(conditions))
      }
    } else {
      base::stop("Please Specify Either UCB1 or Thomspon for algorithm")
    }

    means <- rlang::set_names(prior_data$success_rate, prior_data$mab_condition)

    new_data <- data |> dplyr::filter(period_number == i)

    for (j in base::seq_along(conditions)) {
      condition <- conditions[j]

      if (verbose) {
        base::cat(base::paste("Treatment", j, condition, "\n"))
      }

      bandit_prob <- base::as.numeric(bandit[condition])

      if (bandit_prob == 0) {
        base::warning("Probability of Assignment is 0")
      }

      m_hat <- means[condition]
      if (m_hat == 0) {
        base::warning("Pr(Success|Treatment) = 0")
      }

      ind_aipw <- dplyr::if_else(
        new_data$mab_condition == condition,
        (new_data$mab_success / bandit_prob) +
          (1 - (1 / bandit_prob)) * m_hat[condition],
        m_hat[condition]
      )

      new_data[[base::paste0("aipw_", condition)]] <- ind_aipw
    }
    data <- dplyr::rows_update(data, new_data, by = rlang::as_string(rlang::ensym(id_col)))
  }
  check <- data |>
    dplyr::summarize(dplyr::across(dplyr::starts_with("aipw_"), ~ base::sum(base::is.na(.x)))) |>
    base::sum()

  if (check != 0) {
    base::warning(paste0(check, " Individual AIPW Scores are NA"))
  }

  return(data)
}

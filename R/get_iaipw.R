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
#'
get_iaipw <- function(mab, periods, algorithm, conditions, verbose) {
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
  data <- mab[[1]]
  conditions <- base::sort(conditions)

  new_cols <- paste0("aipw_", conditions)
  data[new_cols] <- NA_real_

  prior_data <- data |>
    dplyr::group_by(period_number, mab_condition) |>
    dplyr::summarize(
      successes = sum(mab_success, na.rm = TRUE),
      trials = dplyr::n(),
      .groups = "drop"
    )


  prior_data <- base::lapply(base::seq_len(periods), function(x) {
    if (verbose) {
      base::cat(base::paste0("Period Number: ", x, "\n"))
    }

    if (x == 1) {
      return(tibble::tibble(
        mab_condition = conditions,
        success_rate = 0
      ))
    } else {
      prior_data <- prior_data |>
        dplyr::filter(period_number < x) |>
        dplyr::group_by(mab_condition) |>
        dplyr::summarize(success_rate = base::sum(successes) / base::sum(trials))
    }

    if (nrow(prior_data) != length(conditions)) {
      add_cond <- tibble::tibble(
        mab_condition = base::setdiff(conditions, prior_data$mab_condition),
        success_rate = 0
      )
      prior_data <- base::rbind(prior_data, add_cond)
    }
    return(prior_data)
  })

  if (algorithm == "UCB1") {
    bandits <- base::lapply(base::seq_len(periods), function(i) {
      prob <- if (i == 1) 1 / base::length(conditions) else 1
      rlang::set_names(rep(prob, base::length(conditions)), base::sort(conditions))
    }) |>
      dplyr::bind_rows(, .id = "period")
  } else {
    bandits <- mab[[2]]
  }

  for (i in seq_len(periods)) {
    mhats <- rlang::set_names(prior_data[[i]]$success_rate, base::sort(prior_data[[i]]$mab_condition))
    subset <- which(data$period_number == i)

    for (j in seq_along(conditions)) {
      condition <- conditions[j]
      bandit_prob <- base::as.numeric(bandits[[condition]][i])

      iaipws <- base::ifelse(
        data$mab_condition[subset] == condition,
        (data$mab_success[subset] / bandit_prob) +
          (1 - 1 / bandit_prob) * mhats[condition],
        mhats[condition]
      )
      data[[new_cols[j]]][subset] <- iaipws
    }
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

#' Gather Past Results for Given Assignment Period
#' @name get_past_results
#' @description Summarizes results of prior periods to use for the current Multi-Arm-Bandit assignment.
#'
#' @inheritParams single_mab_simulation
#' @inheritParams create_prior
#' @inheritParams cols
#' @param current_data Data with only observations from the current sampling period; if `data` is a data.table,
#' this is the whole dataset.
#' @param prior_data Data with only the observations from the prior index; only used when `data` is not a data.table
#' @param current_period current_period of the simulation.
#' @param prior vector of prior periods to use for bandit calculations.
#' @returns A data.frame, containing the number of successes, and number of people for each
#' treatment condition.
#'
#' @seealso
#' *[run_mab_trial()]
#' *[single_mab_simulation()]
#' *[get_bandit()]
#'
#'
get_past_results <- function(current_data, prior_data = NULL, perfect_assignment, assignment_date_col = NULL,
                             conditions, current_period = NULL, prior = NULL) {
  base::UseMethod("get_past_results")
}

#----------------------------------------------------------------------------------
#' @method get_past_results tbl_df
#' @title
#' [get_past_results()] for tibbles
#' @inheritParams get_past_results


get_past_results.tbl_df <- function(current_data, prior_data, perfect_assignment, assignment_date_col = NULL,
                                    conditions, current_period = NULL, prior = NULL) {
  if (!perfect_assignment) {
    current_date <- base::max(current_data[[assignment_date_col$name]])



    prior_data$known_success <- base::ifelse(
      current_date >= prior_data[["new_success_date"]] & !base::is.na(prior_data[["new_success_date"]]),
      1, 0
    )
  } else {
    prior_data$known_success <- prior_data$mab_success
  }

  prior_data <- prior_data |>
    dplyr::group_by(mab_condition) |>
    dplyr::summarize(
      successes = base::sum(known_success, na.rm = TRUE),
      success_rate = base::mean(known_success, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::ungroup()

  if (base::nrow(prior_data) != base::length(conditions)) {
    conditions_add <- base::setdiff(conditions, prior_data$mab_condition)

    replace <- tibble::tibble(
      mab_condition = conditions_add, successes = 0,
      success_rate = 0, n = 0
    )

    prior_data <- dplyr::bind_rows(prior_data, replace)
    prior_data <- prior_data[order(prior_data$mab_condition), ]
  }
  return(prior_data)
}
#------------------------------------------------------------------------------

#' @method get_past_results data.table
#' @title
#' [get_past_results()] for data.tables
#' @inheritParams get_past_results


get_past_results.data.table <- function(current_data, current_period,
                                        prior,
                                        perfect_assignment, assignment_date_col = NULL,
                                        conditions) {
  if (!perfect_assignment) {
    current_date <- base::max(current_data[
      period_number == current_period,
      base::get(assignment_date_col$name)
    ])

    current_data[period_number %in% prior, known_success := data.table::fifelse(
      current_date >= new_success_date &
        !is.na(new_success_date), 1, 0
    )]
  } else if (perfect_assignment) {
    current_data[period_number %in% prior, known_success := mab_success]
  } else {
    rlang::abort("Specify Logical for `perfect_assignment`")
  }

  past_results <- current_data[, .(
    successes = base::sum(known_success, na.rm = TRUE),
    success_rate = base::mean(known_success, na.rm = TRUE),
    n = .N
  ), by = mab_condition]

  current_data[, known_success := NULL]


  if (base::nrow(past_results) != base::length(conditions)) {
    conditions_add <- base::setdiff(conditions, past_results$mab_condition)
    replace <- data.table::data.table(
      mab_condition = conditions_add, successes = 0,
      success_rate = 0, n = 0
    )

    past_results <- data.table::rbindlist(list(past_results, replace),
      use.names = TRUE
    )

    data.table::setorder(past_results, mab_condition)
  }
  return(invisible(past_results))
}

#-------------------------------------------------------------------------------
#' @method get_past_results data.frame
#' @title
#' [get_past_results()] for data.frames
#' @inheritParams get_past_results
get_past_results.data.frame <- function(current_data, prior_data,
                                        perfect_assignment, assignment_date_col = NULL,
                                        conditions, current_period = NULL, prior = NULL) {
  past_results <- get_past_results.tbl_df(
    current_data = tibble::as_tibble(current_data),
    prior_data = tibble::as_tibble(prior_data),
    perfect_assignment = perfect_assignment,
    assignment_date_col = assignment_date_col,
    conditions = conditions,
    current_period = current_period,
    prior = prior
  )
  return(past_results)
}

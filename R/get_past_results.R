#' Gather Past Results for Given Assignment Period
#' @name get_past_results
#' @description This Function Summarizes the results of prior periods to be used for next Multi-Arm Bandit
#' Sampling period.
#'
#' @inheritParams single_mab_simulation
#' @inheritParams create_prior
#' @param current_data Data with only observations from the current re sampling period
#' @param prior_data Data with only the observations from the prior index
#'
#' @returns A data.frame, containing the number of successes, and number of people for each
#' treatment condition.
#'
#' @seealso
#' *[run_mab_trial()]
#' *[single_mab_simulation()]
#' *[mab_sample()]
#'
#'
get_past_results <- function(current_data, prior_data, perfect_assignment, assignment_date_col = NULL,
                             success_date_col = NULL, conditions) {
  base::UseMethod("get_past_results")
}

#----------------------------------------------------------------------------------
#' @method get_past_results tbl_df
#' @title
#' [get_past_results()] for tibbles
#' @inheritParams get_past_results


get_past_results.tbl_df <- function(current_data, prior_data, perfect_assignment, assignment_date_col = NULL,
                                    success_date_col = NULL, conditions) {
  if (!perfect_assignment) {
    current_date <- base::max(dplyr::pull(current_data, {{ assignment_date_col }}), na.rm = TRUE)

    past_results <- prior_data |>
      dplyr::mutate(known_success = dplyr::if_else(
        current_date >= {{ success_date_col }} & !base::is.na({{ success_date_col }}),
        1, 0
      ))
  } else {
    past_results <- prior_data |>
      dplyr::mutate(known_success = mab_success)
  }

  past_results <- past_results |>
    dplyr::group_by(mab_condition) |>
    dplyr::summarize(
      successes = base::sum(known_success, na.rm = TRUE),
      success_rate = base::mean(known_success, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::ungroup()

  if (base::nrow(past_results) != base::length(conditions)) {
    conditions_add <- base::setdiff(conditions, past_results$mab_condition)

    replace <- tibble::tibble(
      mab_condition = conditions_add, successes = 0,
      success_rate = 0, n = 0
    )

    past_results <- base::rbind(past_results, replace)
    past_results <- past_results[order(past_results$mab_condition), ]
  }
  return(past_results)
}
#------------------------------------------------------------------------------

#' @method get_past_results data.table
#' @title
#' [get_past_results()] for data.tables
#' @inheritParams get_past_results


get_past_results.data.table <- function(current_data, prior_data,
                                        perfect_assignment, assignment_date_col = NULL,
                                        success_date_col = NULL, conditions) {
  if (!perfect_assignment) {
    assignment_date_col_name <- rlang::as_name(rlang::enquo(assignment_date_col))
    success_date_col_name <- rlang::as_name(rlang::enquo(success_date_col))

    current_date <- base::max(current_data[, get(assignment_date_col_name)])

    prior_data[, known_success := data.table::fifelse(
      current_date >= get(success_date_col_name) &
        !is.na(get(success_date_col_name)), 1, 0
    )]
  } else if (perfect_assignment) {
    prior_data[, known_success := mab_success]
  } else {
    rlang::abort("Specify Logical for `perfect_assignment`")
  }

  past_results <- prior_data[, .(
    successes = base::sum(known_success, na.rm = TRUE),
    success_rate = base::mean(known_success, na.rm = TRUE),
    n = .N
  ), by = mab_condition]

  if (base::nrow(past_results) != base::length(conditions)) {
    conditions_add <- base::setdiff(conditions, past_results$mab_condition)
    replace <- data.table::data.table(
      mab_condition = conditions_add, successes = 0,
      success_rate = 0, n = 0
    )

    past_results <- data.table::rbindlist(list(past_results, replace))

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
                                        success_date_col = NULL, conditions) {
  past_results <- get_past_results.tbl_df(
    current_data = tibble::as_tibble(current_data),
    prior_data = tibble::as_tibble(prior_data),
    perfect_assignment = perfect_assignment,
    success_date_col = {{ success_date_col }},
    assignment_date_col = {{ assignment_date_col }},
    conditions = conditions
  )
  return(past_results)
}

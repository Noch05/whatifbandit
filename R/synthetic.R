#' Generate Synthetic RCT Data for MAB Re-Simulation
#' @name generate_rtc.bernoulli
#' @description Generates compatible `tibble` or `data.table` for use in `single_mab_simulation()`
#' based on specified input parameters.
#' @param n Number of observations to generate.
#' @param t Number of treatments.
#' @param p Numeric vector of length `t` containing true probabilities of success for each treatment. `names(p)` provide the names of the treatment groups.
#' @param dt Logical; whether to return a `data.table` or a `tibble`. Default is `FALSE`
#' @param simple; Logical; Whether to use simple random assignment or complete random assignment. Default is `TRUE`
#' @param dates_of_assignment Optional `Date` vector containing dates of assignment for each observation.
#'  If its length is less than `n` provided dates will be recylced.
#' @param time_model Optional user-specified function that models the time from treatment until success for successful observations.
#'   The function must return a `period` object. It should accept treatment assignments and outcomes as its first two arguments,
#'   respectively. Additional arguments may be supplied via `...`.
#' @param ... Additional arguments to `time_model`.
#' @returns `tibble` or `data.table` with `n` rows, containing the following columns:
#' \itemize{
#' \item `assignment_dates`: Dates of assignment for each observation.
#' \item `treatments`: Assigned treatment for each observation.
#' \item `success`: Whether or not treatment was successful for each observation.
#' \item `success_dates`: Dates of success for each observation calculated as `assignment_dates + time_model(treatments, success, ...)`.
#' }
#' @export

generate_rct.bernoulli <- function(
  n,
  t,
  p,
  simple = TRUE,
  dt = FALSE,
  dates_of_assignment = NULL,
  time_model = NULL,
  ...
) {
  assign_func <- if (simple) randomizr::simple_ra else randomizr::complete_ra

  treatments <- assign_func(N = n, num_arms = t, conditions = names(p))
  success <- rbinom(n, 1, prob = p[assignments])

  result_func <- if (!dt) tibble::tibble else data.table::data.table

  assignment_dates <- if (is.null(dates_of_assignment)) {
    NULL
  } else if (length(dates_of_assignment) < n) {
    sort(rep_len(dates_of_assignment, n))
  } else {
    dates_of_assignment
  }

  success_dates <- if (is.null(time_model)) {
    NULL
  } else {
    assignment_dates + time_model(treatments, success, ...)
  }

  return(
    result_func(
      assignment_dates,
      treatments,
      success,
      success_dates
    )
  )
}

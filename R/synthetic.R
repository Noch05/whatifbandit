#' Generate Synthetic RCT Data for MAB Re-Simulation
#' @name generate_rtc.bernoulli
#' @description Generates compatible `tibble` or `data.table` for use in `single_mab_simulation()`
#' based on specified input parameters.
#' @param n Number of observations to generate.
#' @param p Numeric vector containing true probabilities of success for each treatment.
#' `names(p)` provide the names of the treatment groups and `length(p)` is the number of treatment arms.
#' @param dt Logical; whether to return a `data.table` or a `tibble`. Default is `FALSE`
#' @param simple; Logical; Whether to use simple random assignment or complete random assignment. Default is `TRUE`
#' @param dates_of_assignment Optional `Date` vector containing dates of assignment for each observation.
#'  If its length is less than `n` provided dates will be recylced.
#' @param time_model Optional user-specified function that models the time from treatment until success for successful observations.
#'   The function must return a `period` object. It should accept `n`, treatment assignments and outcomes as its first 3 arguments,
#'   respectively. Additional arguments may be supplied via `...`.
#' @param ... Additional arguments to `time_model`.
#' @returns `tibble` or `data.table` with `n` rows, containing the following columns:
#' \itemize{
#' \item `assignment_dates`: Dates of assignment for each observation.
#' \item `treatments`: Assigned treatment for each observation.
#' \item `success`: Whether or not treatment was successful for each observation.
#' \item `success_dates`: Dates of success for each observation calculated as `assignment_dates + time_model(treatments, success, ...)`.
#' \item `id`: Row number.
#' }
#' @export
#' @example inst/examples/generate_rct.bernoulli_example.R

generate_rct.bernoulli <- function(
  n,
  p,
  simple = TRUE,
  dt = FALSE,
  dates_of_assignment = NULL,
  time_model = NULL,
  ...
) {
  check_posint(n)
  if (any(p > 1 | p < 0)) {
    rlang::abort(
      c(
        "all `p` must be probabilities between 0 and 1",
        "x" = paste0("You passed:", paste0(p, collapse = ","))
      )
    )
  }

  assign_func <- if (simple) {
    randomizr::simple_ra
  } else {
    randomizr::complete_ra
  }

  if (is.null(names(p))) {
    names(p) <- paste0("T", seq_along(p))
  }

  treatments <- assign_func(
    N = n,
    prob_each = rep(1 / length(p), length(p)),
    conditions = names(p)
  )

  success <- stats::rbinom(n, 1, prob = p[treatments])

  assignment_dates <- NULL
  if (!is.null(dates_of_assignment)) {
    assignment_dates <- if (length(dates_of_assignment) < n) {
      base::sort(base::rep_len(dates_of_assignment, n))
    } else {
      dates_of_assignment
    }
  }

  success_dates <- NULL
  if (!is.null(time_model) && !is.null(assignment_dates)) {
    success_dates <- assignment_dates +
      time_model(n, treatments, success, ...)
  }

  result_func <- if (dt) data.table::data.table else tibble::tibble

  result <- result_func(
    assignment_date = assignment_dates,
    treatment = treatments,
    success = success,
    success_date = success_dates,
    id = 1:n
  )

  return(result)
}

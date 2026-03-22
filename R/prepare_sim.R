#' Generate Start and End Indexes
#' @description
#' Generates the start and end indexes for each period based on provided information
#' @name gen_period_idx
#' @inheritParams simulate_mab
#' @returns list of numeric vectors featuring start and end indexes for each period of the simulation

gen_period_idx <- function(n, t, period_sizes = NULL) {
  period_sizes <-
    if (!is.null(period_sizes)) {
      period_sizes
    } else {
      period_sizes <- c(rep(floor(n / t), t - 1), n %% t)
      if (period_sizes[t] == 0) {
        period_sizes[t] <- period_sizes[t - 1]
      }
      period_sizes
    }
  ends <- cumsum(period_sizes)
  starts <- c(1, ends[-t] + 1)
  return(list(
    start_idxs = starts,
    end_idxs = ends
  ))
}

#' Generate Assignment Dates
#' @description
#' Generates a `length(n)` vector of assignment dates based on provided information.
#' @inheritParams simulate_mab
#' @returns vector of assignment dates

gen_assignment_dates <- function(n, assignment_dates) {
  if (is.null(assignment_dates)) {
    NULL
  } else if (length(assignment_dates) < n) {
    sort(rep_len(assignment_dates, n))
  } else {
    assignment_dates
  }
}

#' Split Function Arguments
#' @name split_args
#' @inheritParams simulate_mab
#' @description
#' Uses [formalArgs()] to match arguments provided to `...` of [simulate_mab()] to [furrr::furrr_options()] and the user specified `time_model`
#' @returns A named list with 2 elements, `furr_args` and `time_model_args` each a list of the respective arguments to
#' [furrr::furrr_options()] and the user specified `time_model`
#'
split_args <- function(...) {
  all_args <- rlang::dots_list(..., .named = TRUE)
  furrr_args <- all_args[
    names(all_args) %in% formalArgs(furrr::furrr_options)
  ]
  time_model_args <- if (!is.null(time_model)) {
    all_args[names(all_args) %in% formalArgs(time_model)]
  } else {
    NULL
  }
  return(list(
    furrr_args = furrr_args,
    time_model_args = time_model_args
  ))
}

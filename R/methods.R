#' @name condense_results
#' @title Condenses results of repeated simulations.
#' @inheritParams mab_from_rct
#' @param dt Logical; Whether to output `data.table`s or `tibble`s. When` r * number_of_periods > 100000`, `dt = TRUE`, even if the user passed data is not a
#' `data.table`.
#' @param mabs List of outputs from repeated [run_mab()] calls.
#' @returns A named list containing
#' \itemize{
#' \item `final_data:` `tibble` or `data.table` containing the nested `tibble`s/`data.table`s from each trial. Only provided when `keep_data = TRUE`.
#' \item `bandits`: A `tibble` or `data.table` containing the UCB1 values or Thompson Sampling posterior distributions for each period and trial. Wide format,
#' each row is a period, and each columns is a treatment.
#' \item `assignment_probs`: A `tibble` or `data.table` containing the probability of being assigned each treatment arm at a given period and trial. Wide format,
#' each row is a period, and each columns is a treatment.
#' \item `estimates`: A `tibble` or `data.table` containing the all estimates and variances for each arm.
#' Long format, treatment arm, and estimate type are columns along with the mean estimates
#' and variance estimates.
#' \item `ipw_vcov`: A 3d arrary containing the covariance matrix of coefficients of IPW estimates of each trial.
#' }
#' @details
#' This function iterates over every element in `mabs` and extracts the required element to place in a condensed list
#' for the final output.
#'
#' @keywords internal

condense_results <- function(dt, keep_data, mabs) {
  r <- length(mabs)
  names(mabs) <- as.character(1:r)
  elements <- c(
    "bandits",
    "assignment_probs",
    "assignment_quantities",
    "estimates"
  )

  extract <- \(item) lapply(mabs, `[[`, item)

  bind_dt <- \(item) {
    if (item == "assignment_quantities") {
      data.table::rbindlist(
        extract(item) |> lapply(as.list),
        idcol = "trial",
        use.names = TRUE
      )[, trial := as.numeric(trial)]
    } else {
      data.table::rbindlist(
        extract(item),
        idcol = "trial",
        use.names = TRUE
      )[, trial := as.numeric(trial)]
    }
  }

  bind_df <- \(item) {
    extract(item) |>
      dplyr::bind_rows(.id = "trial") |>
      dplyr::mutate(trial = as.numeric(trial))
  }

  bind_func <- if (dt) bind_dt else bind_df
  nest_func <- if (dt) {
    \() {
      data.table::data.table(
        trial = seq_len(r),
        data = list(extract("final_data"))
      )
    }
  } else {
    \() tibble::tibble(trial = seq_len(r), data = extract("final_data"))
  }
  results <- lapply(elements, bind_func)
  names(results) <- elements
  results[["final_data"]] <- if (keep_data) nest_func() else NULL

  results[["ipw_vcov"]] <- extract("ipw_vcov") |>
    unlist() |>
    array(dim = c(dim(mabs[[1]][["ipw_vcov"]]), r))

  return(results)
}


#------------------------------------------------------------
#' Constructs `mab` and its other class variants
#' @name construct_mab
#' @description Simple construction for proper `mab` subclasses as output
#' to [simulate_mab()] and [mab_from_rct()].
#' @param mab Named list output of [simulate_mab()] or [mab_from_rct()].
#' @param type Type of simulated trial, either `"rct"` or `"param"` to denote whether it was an rct re-simulation or an simulation form population parameters.
#' @param multi Logical; `TRUE` denotes multiple trials.
#' @returns Input `mab` with appropriate S3 class, restructured for output
#' @keywords internal

construct_mab <- function(mab, type, multi) {
  class <- if (multi) {
    c(paste0("multi_", type, "_mab"), "multi_mab", ".mab")
  } else {
    c(paste0("single_", type, "_mab"), "single_mab", ".mab")
  }
  structure(
    list(
      new_data = mab$final_data,
      bandit = list(
        statistic = mab$bandits,
        assignment_prob = mab$assignment_prob,
        assignment_quant = mab$assignment_quantities
      ),
      estimates = list(point = mab$estimates, vcov = mab$ipw_vcov),
      config = list(args = mab$args, call = mab$call, parallel = mab$furrr)
    ),
    class = class
  )
}

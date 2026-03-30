#-------------------------------------------------------------------------------
#' Column as a Named Vector
#' @name as_named_vec
#' @description
#' Converts `data.frame` column into a vector using another column as the names for the vector
#' @param df `data.frame` used.
#' @param val Column name of values
#' @param name Column value of names
#' @returns vector with values `val` and names `name`
#'
#' @keywords internal
as_named_vec <- function(df, val, name) {
  x <- df[[val]]
  names(x) <- df[[name]]
  x
}

#------------------------------------------------------------------------------
#' Formula Parser
#' @description
#' Parsers the input formula for [mab_from_rct()]
#' @name formula_parse
#' @inheritParams mab_from_rct
#' @returns List of columns specified from formula.
#' @keywords internal

formula_parse <- function(formula) {
  formula <- as.character(formula)

  outcome <- formula[2]

  obc <- strsplit(formula[3], "\\+") |>
    lapply(trimws) |>
    unlist()

  conditions_col <- obc[1]
  other_vars <- lapply(
    list(
      obc[grepl("block\\((.*?)\\)", obc)],
      obc[grepl("cluster\\((.*?)\\)", obc)]
    ),
    gather_args
  )

  parsed <- list(
    condition_col = conditions_col,
    success_col = outcome,
    block_cols = block(other_vars[[1]][["args"]]),
    cluster_col = cluster(other_vars[[2]][["args"]])
  )

  return(parsed)
}
#' Gather Args
#' @description Helper for formula parsing. Parses the expression, and splits the function call from the arguments.
#' @param x String representing an `R` expression, like `"block(x1)"`.
#' @returns A list containing the function call, and the arguments so `"Block(x1)"` gets turned into a list
#' with elements `block, "x1"`.

gather_args <- function(x) {
  if (length(x) == 0) {
    return(list(NULL))
  }
  call <- rlang::parse_expr(x) |>
    as.list()

  args <- vapply(
    call[-1],
    rlang::as_label,
    character(1)
  )
  return(list(call = call[[1]], args = args))
}

# Helpers not requiring documentation, simply identity functions for block and cluster cases.

block <- function(...) {
  c(...)
}
cluster <- function(x) {
  x
}

#' Verbose Printer
#' @description Shorthand Function for checking `verbose` and then printing if TRUE
#' @name verbose_log
#' @param message The message to be printed to screen, as a string.
#' @param log Logical; Whether or not to print the message, this will always be
#' the `verbose` argument passed from higher functions.
#' @returns Text output of `message` to the console when `log = TRUE`. If
#' `log = FALSE`, returns nothing.
#' @keywords internal

verbose_log <- function(log, message) {
  if (log) {
    cat(message, "\n")
  }
}


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
  elements <- c(
    "bandits",
    "assignment_probs",
    "assignment_quantities",
    "estimates"
  )
  r <- length(mabs)
  extract <- \(item) lapply(mabs, `[[`, item)

  bind_dt <- \(item) {
    data.table::rbindlist(
      extract(item),
      idcol = "trial",
      use.names = TRUE
    )[, trial := as.numeric(trial)]
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

#' Convert Treatment Block Summary to Matrix
#' @name summary_to_matrix
#' @description Converts a summarized data.frame or data.table containing
#' `treatment_block`, `success_rate`, and `failure_rate` columns into a
#' named matrix for use with [randomizr::block_ra()].
#' @param df A `data.frame` or `data.table` with columns `treatment_block`,
#' `success_rate`, and `failure_rate`.
#' @returns A numeric matrix with row names equal to `treatment_block` and
#' columns `failure_rate` and `success_rate`.
#' @keywords internal
summary_to_matrix <- function(df) {
  matrix(
    c(df[["failure_rate"]], df[["success_rate"]]),
    ncol = 2,
    nrow = nrow(df),
    dimnames = list(df[["treatment_block"]], c("failure_rate", "success_rate"))
  )
}

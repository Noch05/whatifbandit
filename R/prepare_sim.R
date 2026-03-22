#' Prepares Data for Simulated MAB
#' @name prep_sim_data
#' @description
#' Prepares data structures for simulated MAB trial
#' @returns `tibble` or `data.table` containing
#' @keywords internal

prep_sim_data <- function(
  n,
  p,
  blocks = NULL,
  clusters = NULL,
  blocking,
  clustering,
  period_idxs,
  assignment_dates = NULL
) {
  df_func <- if (dt) data.table::data.table else tibble::tibble

  blocks_clusters <- generate_groups(
    n = n,
    blocks = blocks,
    clusters = clusters
  )
  period_number <- findInterval(seq_len(n), period_idxs[["start_idxs"]])
  current_idx <- period_idxs[["start_idxs"]][1]:period_idxs[["end_idxs"]][1]

  data <- df_func(
    period_number = period_number,
    block = blocks,
    cluster = clusters,
    assignment_dates = assignment_dates,
    mab_condition = NA,
    mab_success = NA,
    new_success_date = NA
  )
  data <- assign_treatments(
    current_data = data[current_idx, ],
    probs = p / length(p),
    blocking = blocking,
    clustering = clustering,
    conditions = names(p),
    random_assign_prop = 0,
    resimulation = FALSE,
    cluster_col = "clusters"
  ) |>
    generate_outcomes(p = p, )
}


#' Generate Block and Cluster Memberships
#' @name generate_groups
#' @description Takes a named probability vector for blocks and clusters and uses
#' [randomizr::complete_ra()] to randomly assign each of `n` units to a
#' blocks and cluster according to those probabilities.
#'
#' @inheritParams simulate_mab
#'
#' @returns A list containing the factor vectors of group assignments for blocks, clusters, both or `NULL` depending on what was specified. Levels
#' for each vector come from the labels for each block and cluster.
#' @keywords internal
generate_groups <- function(n, blocks = NULL, clusters = NULL) {
  supplied_groups <- list(blocks = blocks, clusters = clusters)
  return_vecs <- list(blocks = NULL, clusters = NULL)

  null_check <- vapply(supplied_groups, is.null, logical(1))

  if (all(null_check)) {
    return(return_vecs)
  } else if (any(null_check)) {
    group <- blocks %||% clusters
    name <- names(supplied_groups)[!null_check]
    return_vec <- randomizr::complete_ra(
      N = n,
      prob_each = group,
      conditions = names(group)
    )
    if (name == "clusters") {
      return_vec <- return_vec[order(return_vec)]
    }
    return_vecs[[name]] <- return_vec

    return(return_vecs)
  } else {
    computed_blocks <- randomizr::complete_ra(
      N = n,
      prob_each = blocks,
      conditions = names(blocks)
    )
    computed_clusters <- vector("character", length = n) |>
      factor(levels = unlist(lapply(clusters, names)))
    for (block in names(clusters)) {
      idx <- block == computed_blocks
      probs <- clusters[[block]]
      computed_clusters[idx] <- randomizr::complete_ra(
        N = length(idx),
        prob_each = probs,
        conditions = names(probs)
      )
    }
    ord <- order(computed_clusters)
    return_vecs[["clusters"]] <- computed_clusters[ord]
    return_vecs[["blocks"]] <- computed_blocks[ord]
    return(return_vecs)
  }
  return(return_vecs)
}


#' Generate Start and End Indexes
#' @description
#' Generates the start and end indexes for each period based on provided information
#' @name generate_period_idx
#' @inheritParams simulate_mab
#' @returns list of numeric vectors featuring start and end indexes for each period of the simulation
#' @keywords internal

generate_period_idx <- function(n, t, period_sizes = NULL) {
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
#' @name generate_assignment_dates
#' @description
#' Generates a `length(n)` vector of assignment dates based on provided information.
#' @inheritParams simulate_mab
#' @returns vector of assignment dates
#' @keywords internal
#'
generate_assignment_dates <- function(n, assignment_dates) {
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
#' @keywords internal

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

#' Simulate an Adaptive Trial With Bernoulli Distributed Outcomes
#' @description Simulates a response-adaptive randomized experiment with Bernoulli
#' distributed outcomes. At each period, observed outcomes are used to update assignment
#' probabilities according to the specified `algorithm`. `algorithm = "static"` is the non-adaptive
#' uniform baseline, where probabilities of being assigned to one treatment is the same as any other.
#' @param n A positive integer. Total number of units to simulate.
#' @param t Total number of assignment periods. Positive integer. Default is `t = n` for pure sequential (one unit per period) assignment.
#' The sizes of each period will be equal as `n %/% t`,
#' except for the last period which will be `n %/% t + n %% t`, when `period_sizes = NULL`.
#' @param p The true probabilities of success for each treatment arm. Specified as an matrix,
#' where `rownames(p)` are the treatment
#' labels, and `colnames(p)` are the cluster or block labels, e.g.
#'       `matrix(c(0.5, 0.3, 0.5, 0.6git ), nrow = 2, ncol = 2, dimnames(list(c("T1", "T2"), c("B1", "B2"))))`.
#'       Probabilities are accessed as `p[treatment, block]`.
#' With blocks and clusters utilize the clusters for the columns because clusters are fully nested in blocks.
#' For no clusters or blocks simply use a matrix with 1 column.
#'
#' @param dt Logical. If `TRUE` returns a [data.table::data.table()]; otherwise returns a [tibble::tibble()]. Default `FALSE`.
#' @param blocks A named numeric vector of block membership probabilities (must sum to 1), where `names(blocks)`
#' are the block labels. Units are assigned to blocks via [randomizr::complete_ra()]. Pass `NULL` (default) for no blocking.
#' @param clusters Cluster membership probabilities. Can be:
#' \describe{
#' \item{Numeric vector}{A named vector where `names(clusters)` are the cluster labels e. g. `C(C1 = 0.4, C2 = 0.6)`.
#' Used when there is not blocking.}
#' \item{Named list of vectors}{A named list where `names(clusters)` are block labels, and each element is a named vector
#' of per-block cluster proportions, e.g.
#' `list(B1 = c(C1 = 0.4, C2=0.6), B2 = c(C3 = 0.2, C4 = 0.8))`
#' Clusters are accessed as `clusters[[block]][cluster]`. Insided each block, cluster proportions must sum to 1, and the same cluster cannot appear in multiple blocks.}
#' }
#' Units are assigned to clusters via [randomizr::complete_ra()]. Pass `NULL` (default) for no clustering.
#' @param assignment_dates An optional `Date` vector of dates representing when units are assigned.
#' If shorter than `n` it is recycled and sorted. If NULL` (default) no assignment dates are recorded.
#' @param time_model An optional function with signature `function(n, conditions, success, blocks = NULL, clusters = NULL, ...)`
#' that returns a vector of [lubridate::period] objects which will then be added to `dates_of_assignment` to produce `success_date`. Used to simulate delayed feedback mechanism
#' during the trial, so outcomes are imperfectly observed. Only used when`dates_of_assignment` is also supplied. Default `NULL`. Other optional arguments CANNOT share names as arguments in [furrr::furrr_options()]
#'
#' @param algorithm Assignment algorithm, determines how probabilities of assignment
#' are updated each period. Either `"thompson"` for Thompson Sampling, `"ucb1"` for
#' the UCB1 algorithm, or `"static"` for uniform, non-adaptive assignment. Not case sensitive.
#' @param period_sizes Numeric vector of `length(t)`, with the specific number of units to be assigned in each period. Used when it is required to assign different numbers of units
#' to treatment across the periods of the trial.
#' @param ... Additional named arguments forwarded to `time_model` and [furrr::furrr_options()].
#'
#' @returns NULL
#' @details
#' When blocking and/or clustering are specified, these assignments will be randomly pregenerated before the start of the adaptive sequential assignment. These arguments allow simulating a trial
#' when there may be hetergenous outcomes across a treatment block or treatment cluster, so different assignment probabilities can be provided for the same treatment, depending on the block and/or cluster
#' of a unit.
#'
#' Clustering is challenging under an adaptive trial, because then the probabilities of assignment being adaptive can have little impact on the new assignments, given that an early treatment assignment to a cluster
#' must remain the same across the whole trial. As such this function assumes clusters do not persist across periods, so are all respecitvely assigned at the same time. If a design is provided, as such periods are
#' too small for the clusters to fit in a period, its possible for assignment to vary within the same cluster in the experiment.
#' @export
#' @example inst/examples/simulate_mab_example.R
simulate_mab <- function(
  n,
  t = n,
  p,
  algorithm,
  blocks = NULL,
  clusters = NULL,
  control_augment = 0,
  random_assign_prop = 0,
  assignment_dates = NULL,
  time_model = NULL,
  period_sizes = NULL,
  prior_periods = NULL,
  discount_rate = 1,
  dt,
  ndraws = 5000,
  r,
  keep_data,
  ...
) {
  algorithm <- tolower(algorithm)
  check_mab_sim(
    n = n,
    t = t,
    p = p,
    algorithm = algorithm,
    blocks = blocks,
    clusters = clusters,
    control_augment = control_augment,
    random_assign_prop = random_assign_prop,
    assignment_dates = assignment_dates,
    time_model = time_model,
    period_sizes = period_sizes,
    prior_periods = prior_periods,
    discount_rate = discount_rate,
    dt = dt,
    ndraws = ndraws,
    r = r,
    keep_data = keep_data
  )

  other_args <- split_args(..., time_model = time_model)
  period_idxs <- generate_period_idx(n = n, t = t, period_sizes = period_sizes)
  assignment_dates <- generate_assignment_dates(
    n = n,
    assignment_dates = assignment_dates
  )
  blocking <- !is.null(blocks)
  clustering <- !is.null(clusters)

  if (r == 1) {} else if (r > 1) {
    opts <- do.call(
      furrr:::furrr_options,
      c(list(seed = TRUE), other_args$furrr_args)
    )

    furrr::future_map(seq_len(1), \() {}, .options = furrr_options())
  }
}
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
  assignment_dates = NULL,
  time_model = NULL,
  time_model_args = NULL,
  dt
) {
  df_func <- if (dt) data.table::data.table else tibble::tibble

  blocks_clusters <- generate_groups(
    n = n,
    blocks = blocks,
    clusters = clusters
  )
  period_number <- findInterval(seq_len(n), period_idxs[["start_idxs"]])
  current_idx <- period_idxs[["start_idxs"]][1]:period_idxs[["end_idxs"]][1]

  cols <- list(
    period_number = period_number,
    block = blocks_clusters[["blocks"]],
    cluster = blocks_clusters[["clusters"]],
    assignment_date = assignment_dates,
    mab_condition = rep(NA_character_, n),
    mab_success = rep(NA_real_, n)
  )

  if (!is.null(time_model)) {
    cols[["new_success_date"]] <- rep(as.Date(NA), n)
  }

  data <- do.call(df_func, cols)

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
    generate_outcomes(
      p = p,
      idx = current_idx,
      data = data,
      time_model = time_model,
      time_model_args = time_model_args
    )
  return(invisible(data))
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
        N = sum(idx),
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


#' Extract Success Probabilities Per-Unit
#' @name extract_success_prob
#' @description Looks up the success probability for each unit given their treatment
#' assignment and, optionally, their block and/or cluster membership. Handles
#' all supported `p` structures.
#'
#' @inheritParams simulate_mab
#' @inheritParams run_mab
#' @param conditions A character or factor vector of treatment assignments of
#'   length `n`.
#' @param other_idx Character vector of block or cluster assigents to be used as the
#' additionnal index for extracting from `p`.
#' @returns A numeric vector of length containing the per-unit success
#'   probabilities to be used for outcome observation.
#' @keywords internal
extract_success_prob <- function(
  p,
  conditions,
  other_idx = NULL
) {
  if (!is.null(other_idx)) {
    extract_mat <- matrix(data = c(conditions, other_idx), ncol = 2)
    p[extract_mat]
  } else {
    return(p[conditions])
  }
}

#' Generate Outcomes Per-Unit
#' @name generate_outcomes
#' @description
#' Uses provided success probabilities to draw a Bernoulli outcome for each unit. If `time_model` is provided, it is also used to compute
#' dates of success
#' @inheritParams impute_outcomes
#' @inheritParams simulate_mab
#' @returns Updated `data` object containing all the outcomes generated in the period, such as the treatment assignments, treatment outcomes. and new success dates

generate_outcomes <- function(
  current_data,
  data,
  p,
  idx,
  time_model = NULL,
  time_model_args = NULL
) {
  conditions <- current_data[["mab_condition"]]
  probs <- extract_success_prob(
    p = p,
    conditions = conditions,
    other_idx = current_data[["cluster"]] %||% current_data[["block"]]
  )
  outcomes <- rbinom(
    nrow(current_data),
    1,
    prob = probs
  )
  success_times <- if (!is.null(current_data[["assignment_date"]])) {
    do.call(
      time_model,
      c(
        list(
          n = nrow(current_data),
          conditions = conditions,
          success = outcomes,
          blocks = current_data[["block"]],
          clusters = current_data[["cluster"]]
        ),
        time_model_args
      )
    )
  } else {
    NULL
  }
  modified_cols <- c("mab_condition", "mab_success")
  if (!is.null(success_times)) {
    current_data[["new_success_date"]] <- current_data[["assignment_date"]] +
      success_times
    modified_cols <- c(modified_cols, "new_success_date")
  }

  if (data.table::is.data.table(data)) {
    data[idx, (modified_cols) := current_data[, ..modified_cols]]
  } else {
    data[idx, ] <- current_data
  }
}


#' Generate Start and End Indexes
#' @description
#' Generates the start and end indexes for each period based on provided information
#' @name generate_period_idx
#' @inheritParams simulate_mab
#' @returns list of numeric vectors featuring start and end indexes for each period of the simulation
#' @keywords internal
#' @details When not provided period sizes are calculated as `n %/% t` for all periods, with the last as `n %/% t  + n %% t`.

generate_period_idx <- function(n, t, period_sizes = NULL) {
  period_sizes <-
    if (!is.null(period_sizes)) {
      period_sizes
    } else {
      size <- floor(n / t)
      period_sizes <- c(rep(size, t - 1), n - (size * (t - 1)))
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

split_args <- function(time_model = NULL, ...) {
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

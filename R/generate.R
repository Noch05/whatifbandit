#' Validate Bernoulli Success Probabilities
#' @name check_probs
#' @description Checks that all provided probabilities are valid between 0 and 1, and that probabilities
#' have been provided for all blocks and clusters
#'
#' @inheritParams extract_success_prob
#' @inheritParams generate_bernoulli.rct
#' @inheritParams assign_treatments.rct
#'
#' @returns Nothing. Throws an error if validation fails.
#'
#' @keywords internal
check_p <- function(p, blocks = NULL, clusters = NULL) {
  flat_p <- base::unlist(p)
  if (base::any(flat_p > 1 | flat_p < 0)) {
    rlang::abort(c(
      "all `p` must be probabilities between 0 and 1",
      "x" = base::paste0("You passed: ", base::paste0(flat_p, collapse = ", "))
    ))
  }
  if (base::is.null(blocks) && base::is.null(clusters)) {
    return(0)
  }

  n_clusters <- base::nlevels(clusters)
  n_blocks <- base::nlevels(blocks)
  n_treat <- base::length(p)

  req_prob <- if (n_clusters > 0) {
    n_treat * n_clusters
  } else if (n_blocks > 0) {
    n_treat * n_blocks
  } else {
    n_treat
  }

  passed_prob <- base::length(flat_p)
  if (passed_prob != req_prob) {
    rlang::abort(c(
      "Not enough probabilities provided",
      "i" = base::sprintf("Required number: %d ", req_prob),
      "x" = base::sprintf("You passed: %d", passed_prob)
    ))
  }
}


#' Generate Block or Cluster Memberships
#' @name generate_group_membership
#' @description Takes a named probability vector for blocks or clusters and uses
#' [randomizr::complete_ra()] to randomly assign each of `n` units to a
#' block or cluster according to those probabilities.
#'
#' @param n A positive integer. Number of units to assign.
#' @param group A named numeric vector or named list (see [generate_rct.bernoulli()]) of assignment probabilities.
#' When blocks and cluster are together, clusters must be fully nested in blocks.
#'   `names(group)` are used as the condition labels (block or cluster names).
#' @inheritParams assign_treatments.rct
#'
#' @returns A factor of length `n` with levels corresponding to `names(group)` or `NULL` if `group = NULL`.
#'
#' @keywords internal
generate_group_membership <- function(n, group, blocks = NULL) {
  if (base::is.null(group)) {
    return(NULL)
  } else {
    if (base::is.list(group)) {
      if (base::is.null(blocks)) {
        rlang::abort("Nested clusters require `blocks` to be specified.")
      }
      if (!base::setequal(base::names(group), base::levels(blocks))) {
        rlang::abort("`names(clusters)` must match block labels.")
      }
      clusters <- base::vector("character", n) |>
        factor(levels = base::unlist(base::lapply(group, names)))
      for (b in base::names(group)) {
        probs <- group[[b]]
        idx <- blocks == b
        clusters[idx] <- randomizr::complete_ra(
          N = base::sum(idx),
          prob_each = probs,
          conditions = base::names(probs)
        )
      }
      return(clusters)
    }
    if (base::is.null(base::names(group))) {
      rlang::abort("`names()` for blocks and/or clusters cannot be `NULL`")
    }
    if (!dplyr::near(base::sum(group), 1)) {
      rlang::abort(
        "Assignment probabilities for blocks and/or clusters must sum to 1"
      )
    }
    return(
      randomizr::complete_ra(
        N = n,
        prob_each = group,
        conditions = base::names(group)
      )
    )
  }
}


#' Assign Treatments
#' @name assign_treatments.rct
#' @description Selects the appropriate [randomizr] randomization function based on whether
#' blocks and/or clusters are present, and returns a vector of treatment
#' assignments.
#'
#' @inheritParams generate_rct.bernoulli
#' @param assignment_probs A numeric vector of equal treatment assignment
#'   probabilities (length = number of treatments, sums to 1).
#' @param blocks A factor or character vector of block memberships, or `NULL`.
#' @param clusters A factor or character vector of cluster memberships, or `NULL`.
#'
#' @returns A factor of length `n` with levels corresponding to treatment arms.
#'
#' @keywords internal
assign_treatments.rct <- function(
  n,
  assignment_probs,
  blocks = NULL,
  clusters = NULL
) {
  undo_randomizr_defaults <- function(treatments, assignment_probs) {
    if (base::is.null(base::names(assignment_probs))) {
      treatments |> as.numeric() |> factor(labels = 1:length(assignment_probs))
    } else {
      treatments
    }
  }
  treatments <- if (!base::is.null(blocks) && !base::is.null(clusters)) {
    randomizr::block_and_cluster_ra(
      blocks = blocks,
      clusters = clusters,
      prob_each = assignment_probs,
      conditions = names(assignment_probs)
    )
  } else if (!base::is.null(blocks)) {
    randomizr::block_ra(
      blocks = blocks,
      prob_each = assignment_probs,
      conditions = names(assignment_probs)
    )
  } else if (!base::is.null(clusters)) {
    randomizr::cluster_ra(
      clusters = clusters,
      prob_each = assignment_probs,
      conditions = names(assignment_probs)
    )
  } else {
    randomizr::complete_ra(n, prob_each = assignment_probs)
  }
  undo_randomizr_defaults(treatments, assignment_probs)
}


#' Extract Success Probabilities Per-Unit
#' @name extract_success_prob
#' @description Looks up the success probability for each unit given their treatment
#' assignment and, optionally, their block and/or cluster membership. Handles
#' all supported `p` structures: plain vector, block-indexed list, cluster-indexed list, and
#' block-then-cluster nested list.
#'
#' @inheritParams generate_rct.bernoulli
#' @param treatments A character or factor vector of treatment assignments of
#'   length `n`.
#' @inheritParams assign_treatments.rct
#'
#' @returns A numeric vector of length `n` containing the per-unit success
#'   probability.
#'
#' @keywords internal
extract_success_prob <- function(
  p,
  treatments,
  blocks = NULL,
  clusters = NULL
) {
  idxs <- stats::setNames(
    base::lapply(list(treatments, blocks, clusters), \(x) {
      char <- base::as.character(x)
      if (base::length(char) > 0) {
        char
      } else {
        NULL
      }
    }),
    c("treatments", "blocks", "clusters")
  )
  if (!base::is.null(blocks) && !base::is.null(clusters)) {
    items <- list(idxs[["treatments"]], idxs[["blocks"]], idxs[["clusters"]])
    purrr::pmap_vec(
      items,
      \(t, b, c) p[[t]][[b]][c],
      .ptype = numeric()
    )
  } else if (!base::is.null(blocks) || !base::is.null(clusters)) {
    items <- base::list(
      idxs[["treatments"]],
      idxs[["blocks"]] %||% idxs[["clusters"]]
    )
    purrr::pmap_vec(
      items,
      \(t, b) p[[t]][b],
      .ptype = numeric()
    )
  } else {
    p[idxs[["treatments"]]]
  }
}

#' Add names to an Unnamed Vector
#' @name add_names
#' @description Adds `names` attribute to an unnamed vector based on the provided prefix.
#' @param x Input vector to be named
#' @param prefix String to be prepended to index of an element to create names. e.g. if "T",
#' `names(x) <- c("T1", "T2",..., "Tn")`
#' @returns Original vector with new names, or the original vector if it was already named.

add_names <- function(x, prefix) {
  if (base::is.null(base::names(x))) {
    base::names(x) <- base::paste0(prefix, base::seq_along(x))
  }
  x
}

#' Generate a Bernoulli RCT dataset
#' @name generate_rct.bernoulli
#' @description Simulates a randomized controlled trial with Bernoulli outcomes. Supports
#' complete, block, cluster, and block-and-cluster randomized assignment, optional
#' assignment dates, and a user-supplied time-to-event model for successful observations.
#'
#'
#' @param n A positive integer. Total number of units to simulate.
#' @param p The true probabilities of success for each treatment arm, where `length(p)` is the number of
#' treatment arms. Can be:
#'   \describe{
#'     \item{Numeric vector}{A named vector where `names(p)` are the treatment
#'       labels, e.g. `c(T1 = 0.2, T2 = 0.4)`. Used when there are no blocks
#'       or clusters.}
#'     \item{Named list of vectors}{A named list where `names(p)` are the treatment
#'        labels and each element is a named vector of per-block or per-cluster
#'       success probabilities, e.g.
#'       `list(T1 = c(B1 = 0.2, B2 = 0.4), T2 = c(B1 = 0.3, B2 = 0.5))`.
#'       Probabilities are accessed as `p[[treatment]][block]`.}
#'     \item{Nested named list}{A doubly-nested named list used when both blocks
#'       and clusters are present. The outer names are treatment labels, the inner
#'       names are block labels, and each inner most vector is a named vector of
#'       per-cluster success probabilities. e.g.
#'       `list(T1 = list(B1 = c(C1 = 0.2, C2 = 0.4), B2 = c(C1 = 0.7, C2 = 0.4))
#'             T2 = list(B1 = c(C1 = 0.5, C2 = 0.5), B2 = c(C1 = 0.3, C2 = 0.6)))`
#'        Accessed as `p[[treatment]][[block]][cluster]`.}
#'   }
#'   All probability values must be between 0 and 1.
#' @param dt Logical. If `TRUE` returns a [data.table::data.table()]; otherwise returns a [tibble::tibble()]. Default `FALSE`.
#' @param blocks A named numeric vector of block membership probabilities (must sum to 1), where `names(blocks)`
#' are the block labels. Units are assigned to blocks via [randomizr::complete_ra()]. Pass `NULL` (default) for no blocking.
#' @param clusters Cluster membership probabilities. Can be:
#' \describe{
#' \item{Numeric vector}{A named vector where `names(clusters)` are the cluster labels e. g. `C(C1 = 0.4, C2 = 0.6)`.
#' Used when there is not blocking.}
#' \item{Named list of vectors}{A named list where `names(clusters)` are block labels, and each element is a named vector
#' of per-block cluster proportions, e.g.
#' `list(B1 = c(C1 = 0.4, C=0.6)), B2 = c(C3 = 0.2, C4 = 0.8))`
#' Clusters are accessed as `clusters[[block]][cluster]`. Insided each block, cluster proportions must sum to 1, and the same cluster cannot appear in multiple blocks.}
#' }
#' Units are assigned to clusters via [randomizr::complete_ra()]. Pass `NULL` (default) for no clustering.
#' @param dates_of_assignment An optional vector of dates representing when units are assigned.
#' If shorter than `n` it is recycled and sorted. If NULL` (default) no assignment dates are recorded.
#' @param time_model An optional function with signature `function(n, treatments, success, blocks = NULL, clusters = NULL, ...)`
#' that returns a vector of [lubridate::period] objects to add to `dates_of_assignment` to produce `success_date`.
#' Only used when`dates_of_assignment` is also supplied. Default `NULL`.
#' @param ... Additional arguments forwarded to `time_model`.
#'
#' @returns A [tibble::tibble()] or [data.table::data.table()] with columns:
#'   \describe{
#'     \item{`id`}{Integer unit identifier.}
#'     \item{`treatment`}{Treatment arm assigned to each unit.}
#'     \item{`success`}{Binary outcome (0/1) drawn from a Bernoulli distribution.}
#'     \item{`assignment_date`}{Date of assignment (if `dates_of_assignment` supplied).}
#'     \item{`success_date`}{Date of outcome (if `dates_of_assignment` and `time_model` supplied).}
#'   }
#'
#' @export
#' @example inst/examples/generate_rct.bernoulli_example.R
generate_rct.bernoulli <- function(
  n,
  p,
  dt = FALSE,
  blocks = NULL,
  clusters = NULL,
  dates_of_assignment = NULL,
  time_model = NULL,
  ...
) {
  check_posint(n)
  check_logical(dt)
  p <- add_names(p, "T")

  blocks <- generate_group_membership(n, blocks)
  clusters <- generate_group_membership(n, clusters, blocks = blocks)

  assignment_probs <- stats::setNames(
    base::rep(1 / base::length(p), base::length(p)),
    names(p)
  )
  treatments <- assign_treatments.rct(
    n = n,
    assignment_probs = assignment_probs,
    blocks = blocks,
    clusters = clusters
  )

  check_p(p, blocks = blocks, clusters = clusters)

  success_prob <- extract_success_prob(
    p = p,
    treatments = treatments,
    blocks = blocks,
    clusters = clusters
  )

  success <- stats::rbinom(n, 1, prob = success_prob)

  assignment_dates <- if (base::is.null(dates_of_assignment)) {
    NULL
  } else if (base::length(dates_of_assignment) < n) {
    base::sort(base::rep_len(dates_of_assignment, n))
  } else {
    dates_of_assignment
  }

  success_dates <- NULL
  if (!base::is.null(time_model) && !base::is.null(assignment_dates)) {
    success_dates <- assignment_dates +
      time_model(
        n = n,
        treatments = treatments,
        success = success,
        blocks = blocks,
        clusters = clusters,
        ...
      )
  }

  result_func <- if (dt) data.table::data.table else tibble::tibble
  result_func(
    id = 1:n,
    treatment = treatments,
    success = success,
    block = blocks,
    cluster = clusters,
    assignment_date = assignment_dates,
    success_date = success_dates
  )
}


#' Simulate an Adaptive Trial With Bernoulli Distributed Outcomes
#' @description Simulates a response-adaptive randomized experiment with Bernoulli
#' distributed outcomes. At each period, observed outcomes are used to update assignment
#' probabilities according to the specified `algorithm`. `algorithm = "static"` is the non-adaptive
#' uniform baseline, where probabilities of being assigned to one treatment is the same as any other.
#' @name mab_trial_sim.bernoulli
#' @inheritParams generate_rct.bernoulli
#' @param t Total number of assignment periods. Positive integer. Default is `t = n` for pure sequential (one unit per period) assignment.
#' The sizes of each period will be equal as `n %/% t`, except for the last
#' period which will be `n %% t` in the case `n %% t != 0`, when `period_sizes = NULL`.
#' @param algorithm Assignment algorithm, determines how probabilities of assignment
#' are updated each period. Either `"thompson"` for Thompson Sampling, `"ucb1"` for
#' the UCB1 algorithm, or `"static"` for uniform, non-adaptive assignment. Not case sensitive.
#' @param period_sizes Numeric vector of `length(t)`, with the specific number of units to be assigned in each period. Used when it is required to assign different numbers of units
#' to treatment across the periods of the trial.
#' @inheritParams single_mab_simulation
#' @details
#' When blocking and/or clustering are specified, these assignments will be randomly pregenerated before the start of the adaptive sequential assignment. These arguments allow simulating a trial
#' when there may be hetergenous outcomes across a treatment block or treatment cluster, so different assignment probabilities can be provided for the same treatment, depending on the block and/or cluster
#' of a unit.
#'
#' Clustering is challenging under an adaptive trial, because then the probabilities of assignment being adaptive can have little impact on the new assignments, given that an early treatment assignment to a cluster
#' must remain the same across the whole trial. As such this function assumes clusters do not persist across periods, so are all respecitvely assigned at the same time. If a design is provided, as such periods are
#' too small for the clusters to fit in a period, its possible for assignment to vary within the same cluster in the experiment.
#' @export
mab_trial_sim.bernoulli <- function(
  n,
  t = n,
  p,
  algorithm,
  blocks = NULL,
  clusters = NULL,
  control_augment = 0,
  random_assign_prop = 0,
  period_sizes = NULL,
  prior_periods = "all",
  dt,
  ndraws = 5000
) {
  check_posint(n, t, prior_periods)
  p <- add_names(p, "T")
  alg <- base::tolower(algorithm)
  if (!alg %in% c("static", "thompson", "ucb1")) {
    rlang::abort(
      message = c(
        "Invalid Assignment Algorithm",
        "x" = base::sprintf("You passed: %s", algorithm),
        "!" = base::sprintf(
          "Valid Algorithms: %s, %s, %s",
          "thompson",
          "ucb1",
          "static"
        )
      )
    )
  }

  blocks <- generate_group_membership(n, blocks)
  clusters <- generate_group_membership(n, clusters, blocks) |> sort()
  check_p(p, blocks = blocks, clusters = clusters)

  period_sizes <- if (!base::is.null(period_sizes)) {
    period_sizes
  } else {
    c(base::rep(base::floor(n / t), t - 1), n %% t)
  }
  period_sizes[t] <- if (period_sizes[t] == 0) {
    period_sizes[t - 1]
  } else {
    period_sizes[t]
  }
  ends <- base::cumsum(period_sizes)
  starts <- c(1, ends[-t] + 1)

  df_func <- if (dt) {
    \(...) {
      data.table::data.table(..., key = "period_number")
    }
  } else {
    tibble::tibble
  }

  df <- df_func(
    assignments = NA_character_,
    outcome = NA_real_,
    p_assignment = NA_real_,
    block = blocks,
    cluster = clusters,
    period_number = base::rep(base::seq_len(t), times = period_sizes)
  )

  return(results)
}

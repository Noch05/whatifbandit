#' Validate Bernoulli Success Probabilities
#' @name check_probs
#' @description Checks that all provided probabilities are valid between 0 and 1.
#'
#' @param p A numeric vector, named list of vectors, or nested named list of vectors
#'   containing success probabilities. See [generate_rct.bernoulli()] for full details.
#'
#' @returns Nothing. Throws an error if validation fails.
#'
#' @keywords internal
check_probs <- function(p) {
  flat_p <- base::unlist(p)
  if (base::any(flat_p > 1 | flat_p < 0)) {
    rlang::abort(c(
      "all `p` must be probabilities between 0 and 1",
      "x" = base::paste0("You passed: ", base::paste0(flat_p, collapse = ", "))
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
#' @param blocks A factor with levels of `names(blocks)` holding block assignment for each observation.
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
      return(as.factor(clusters))
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
#' @name assign_treatments
#' @description Selects the appropriate [randomizr] randomization function based on whether
#' blocks and/or clusters are present, and returns a vector of treatment
#' assignments.
#'
#' @param n A positive integer. Number of units.
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
#' @param p A numeric vector, named list of vectors, or nested named list of
#'   vectors. See [generate_rct.bernoulli()] for full details.
#' @param treatments A character or factor vector of treatment assignments of
#'   length `n`.
#' @param blocks A character or factor vector of block memberships of length
#'   `n`, or `NULL`.
#' @param clusters A character or factor vector of cluster memberships of
#'   length `n`, or `NULL`.
#' @param n_treatments Integer. Number of treatment arms, used to auto-name
#'   unnamed probability vectors.
#'
#' @returns A numeric vector of length `n` containing the per-unit success
#'   probability.
#'
#' @keywords internal
extract_success_prob <- function(
  p,
  treatments,
  blocks = NULL,
  clusters = NULL,
  n_treatments
) {
  if (!base::is.null(blocks) && !base::is.null(clusters)) {
    purrr::pmap_vec(
      base::list(treatments, blocks, clusters),
      \(t, b, c) p[[t]][[b]][c],
      .ptype = numeric()
    )
  } else if (!base::is.null(blocks)) {
    purrr::map2_vec(
      treatments,
      blocks,
      \(t, b) p[[t]][b],
      .ptype = numeric()
    )
  } else if (!base::is.null(clusters)) {
    purrr::map2_vec(
      treatments,
      clusters,
      \(t, c) p[[t]][c],
      .ptype = numeric()
    )
  } else {
    p[treatments]
  }
}


#' Generate a Bernoulli RCT dataset
#' @name generate_rct.bernoulli
#' @description Simulates a randomized controlled trial with Bernoulli outcomes. Supports
#' complete, block, cluster, and block-and-cluster randomized assignment, optional
#' assignment dates, and a user-supplied time-to-event model for successful observations.
#'
#' @param n A positive integer. Total number of units to simulate.
#' @param p The true probabilities of success for each treatment arm. Can be:
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
#' `list(B1 = c(C1 = 0.4, C=0.6)), B2 = c(C3 = 0.2, C4 = 0.8)`
#' Clusters are accessed as `clusters[[block]][cluster]`. Insided each block, cluster proportions must sum to 1, and clusters across blocks
#' should be different.}
#' }
#' where `names(clusters)` are the cluster labels.
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
  check_probs(p)

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

  success_prob <- extract_success_prob(
    p = p,
    treatments = treatments,
    blocks = blocks,
    clusters = clusters,
    n_treatments = base::length(p)
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
    blocks = blocks,
    clusters = clusters,
    assignment_date = assignment_dates,
    success_date = success_dates
  )
}

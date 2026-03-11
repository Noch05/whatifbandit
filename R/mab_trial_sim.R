#' Simulate an Adaptive Trial With Bernoulli Distributed Outcomes
#' @description Simulates a response-adaptive randomized experiment with Bernoulli
#' distributed outcomes. At each period, observed outcomes are used to update assignment
#' probabilities according to the specified `algorithm`. `algorithm = "static"` is the non-adaptive
#' uniform baseline, where probabilities of being assigned to one treatment is the same as any other.
#' @param n A positive integer. Total number of units to simulate.
#' @param t Total number of assignment periods. Positive integer. Default is `t = n` for pure sequential (one unit per period) assignment.
#' The sizes of each period will be equal as `n %/% t`,
#' except for the last period which will be `n %% t` in the case `n %% t != 0`, when `period_sizes = NULL`.
#' @param p The true probabilities of success for each treatment arm. Specified as an matrix,
#' where `rownames(p)` are the treatment
#' labels, and `colnames(p)` are the cluster or block labels, e.g.
#'       `matrix(c(0.5, 0.3, 0.5, 0.6), nrow = 2, ncol = 2, dimnames(list(c("T1", "T2"), c("B1", "B2"))))`.
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
#' @param dates_of_assignment An optional vector of dates representing when units are assigned.
#' If shorter than `n` it is recycled and sorted. If NULL` (default) no assignment dates are recorded.
#' @param time_model An optional function with signature `function(n, treatments, success, blocks = NULL, clusters = NULL, ...)`
#' that returns a vector of [lubridate::period] objects to add to `dates_of_assignment` to produce `success_date`.
#' Only used when`dates_of_assignment` is also supplied. Default `NULL`.
#'
#' @param algorithm Assignment algorithm, determines how probabilities of assignment
#' are updated each period. Either `"thompson"` for Thompson Sampling, `"ucb1"` for
#' the UCB1 algorithm, or `"static"` for uniform, non-adaptive assignment. Not case sensitive.
#' @param period_sizes Numeric vector of `length(t)`, with the specific number of units to be assigned in each period. Used when it is required to assign different numbers of units
#' to treatment across the periods of the trial.
#' @param ... Additional arguments forwarded to `time_model`.
#' @inheritParams mab_from_rct.bernoulli
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
#' @example inst/examples/mab_trial_sim.bernoulli_example.R
mab_trial_sim.bernoulli <- function(
  n,
  t = n,
  p,
  algorithm,
  blocks = NULL,
  clusters = NULL,
  control_augment = 0,
  random_assign_prop = 0,
  dates_of_assignment,
  time_model = NULL,
  period_sizes = NULL,
  prior_periods = NULL,
  dt,
  ndraws = 5000,
  r,
  keep_data,
  ...
) {
  algorithm <- base::tolower(algorithm)

  data <- prepare_sim(
    p = p,
    blocks = blocks,
    clusters = clusters,
    control_augment,
  )

  blocks <- generate_group_membership(n, blocks) |> base::as.character()
  clusters <- generate_group_membership(n, clusters, blocks = blocks) |>
    base::as.character()
  blocking <- !base::is.null(blocks)
  clustering <- !base::is.null(clusters)

  check_p(p, blocks = blocks, clusters = clusters)

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
    mab_condition = NA_character_,
    outcome = NA_real_,
    block = blocks,
    cluster = clusters,
    period_number = base::rep(base::seq_len(t), times = period_sizes)
  )

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

#' Perform Validation Checks for [mab_trial_sim.bernoulli()]
#' @name check_sim
#' @inheritParams mab_trial_sim.bernoulli
#' @returns Nothing; Throws an error if checks are not met
#' @keywords internal
check_sim <- function(
  n,
  t = n,
  p,
  algorithm,
  blocks,
  clusters,
  control_augment,
  random_assign_prop,
  dates_of_assignment,
  time_model,
  period_sizes,
  prior_periods,
  dt,
  ndraws = 5000,
  r,
  keep_data
) {
  check_logical(dt, keep_data)
  check_posint(n, t, ndraws, r, prior_periods)
  check_prop(control_augment, random_assign_prop)
  check_p(p = p, blocks = blocks, clusters = clusters)
  if (!base::is.null(blocks) && !base::is.null(clusters)) {
    base::do.call(check_sum1, c(list(blocks), clusters))
  } else if (!base::is.null(clusters)) {
    check_sum1(clusters = clusters)
  } else if (!base::is.null(blocks)) {
    check_sum1(blocks = blocks)
  }

  if (!base::is.null(time_model) && !base::is.function(time_model)) {
    rlang::abort("`time_model` must be a function")
  }

  if (t > n) {
    rlang::abort(
      c("`t` cannot be larger than `n`"),
      "x" = sprintf("You Passed: t: %d, n: %d", t, n)
    )
  }

  if (!algorithm %in% c("static", "thompson", "ucb1")) {
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
}
#' Validates Summing to 1
#' @name check_sum1
#' @description
#' Checks specified numeric vector sums to 1, throws an error if not
#' @param ... Arguments to check.
#' @returns Nothing; Throws an error if the check fails
check_sum1 <- function(...) {
  args <- rlang::dots_list(..., named = TRUE)
  purrr::iwalk(args, \(arg, name) {
    if (sum(arg) != 1) {
      rlang::abort(c(
        base::sprintf("`%s` must sum to 1", name),
        "x" = base::paste0("You passed: ", base::paste0(arg, collapse = ", ")),
        "x" = base::paste0("Sum: ", base::sum(arg))
      ))
    }
  })
}


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
  if (base::any(p > 1 | p < 0)) {
    rlang::abort(c(
      "all `p` must be probabilities between 0 and 1",
      "x" = base::paste0("You passed: ", base::paste0(p, collapse = ", "))
    ))
  }
  if (base::is.null(blocks) && base::is.null(clusters)) {
    return(0)
  }

  n_clusters <- base::length(base::unique(clusters))
  n_blocks <- base::length(base::unique(blocks))
  n_treat <- base::dim(p)[1] %|% length(p)

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
#' @param group A named numeric vector or named list (see [mab_trial_sim.bernoulli()]) of assignment probabilities.
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


#' Extract Success Probabilities Per-Unit
#' @name extract_success_prob
#' @description Looks up the success probability for each unit given their treatment
#' assignment and, optionally, their block and/or cluster membership. Handles
#' all supported `p` structures.
#'
#' @inheritParams mab_trial_sim.bernoulli
#' @inheritParams mab_simulate
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
  clusters = NULL,
  blocking,
  clustering
) {}

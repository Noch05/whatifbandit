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
#' @param assignment_dates An optional `Date` vector of dates representing when units are assigned.
#' If shorter than `n` it is recycled and sorted. If NULL` (default) no assignment dates are recorded.
#' @param time_model An optional function with signature `function(n, conditions, success, blocks = NULL, clusters = NULL, ...)`
#' that returns a vector of [lubridate::period] objects to add to `dates_of_assignment` to produce `success_date`.
#' Only used when`dates_of_assignment` is also supplied. Default `NULL`. Other optional arguments CANNOT share names as arguments in [furrr::furrr_options()]
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

  other_args <- split_args(...)
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
#' @inheritParams impute_
#' @inheritParams simulate_mab
#' @returns updated `data.table`

generate_outcomes <- function(
  current_data,
  full_data,
  success_probs,
  starts,
  ends,
  current_period,
  time_model = NULL,
  time_model_args = NULL
) {}

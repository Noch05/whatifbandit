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

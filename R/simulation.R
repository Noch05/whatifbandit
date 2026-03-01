#' Simulate an Adaptive Trial With Bernoulli Distributed Outcomes
#' @description Simulates a response-adaptive randomized experiment with Bernoulli
#' distributed outcomes. At each period, observed outcomes are used to update assignment
#' probabilities according to the specified `algorithm`. `algorithm = "static"` is the non-adaptive
#' uniform baseline, where probabilities of being assigned to one treatment is the same as any other.
#' @name adaptive_trial_sim.bernoulli
#' @param n Total number of units to simulate. Positive integer.
#' @param t Total number of assignment periods. Positive integer. Default is `t = n` for pure sequential (one unit per period) assignment.
#' The sizes of each period will be equal as `n // t`, except for the last
#' period which will be `n %% t` in the case `n %% t != 0`.
#' @param p Named numeric vector of true success probabilities for each arm. All values should be between 0 and 1.
#' @param algorithm Assignment algorithm, determines how probabilities of assignment
#' are updated each period. Either `"thompson"` for Thompson Sampling, `"ucb1"` for
#' the UCB1 algorithm, or `"static"` for uniform, non-adaptive assignment. Not case sensitive
adaptive_trial_sim.bernoulli <- function(n, t = n, p, algorithm) {
  purrr::walk(list(n, t), check_posint)
  check_p(p)
  p <- add_names(p, "T")
  alg <- tolower(algorithm)
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
}

#' Gather Past Results for Given Assignment Period
#' @name compute_prior
#' @description Summarizes results of prior periods to update assignment probabilities in the current period. This function
#' calculates the number of success under each treatment and the total number of observations assigned to each treatment which are used
#' to calculate UCB1 values or Thompson sampling probabilities. These values are weighted by the discount_rate provided.
#'
#' @inheritParams run_mab
#' @param current_data A `data.frame` or `data.table` with only observations from the current sampling period.
#' @param prior_data A `data.frame` or `data.table` with only the observations from the prior index.
#' @returns A list containing 2 named vectors: the weighted number of successes, and the weighted number of assignments, where the names of each vector
#' correspond to the treatment condition.
#'
#' @details
#' When `delayed_feedback = TRUE`, the maximum value from the specified
#' `assignment_date_col` in the current data is taken as the last possible date
#' the researchers conducting the experiment could have learned about a treatment outcome.
#' All successes that occur past this date are masked and treated as failures for the purposes
#' of assigning this treatments periods, as it simulates the researchers not having
#' received that information yet.
#'
#'
#' @seealso
#' * [mab_loop()]
#' * [compute_bandit()]
#' @keywords internal
compute_prior <- function(
  current_data,
  prior_data,
  delayed_feedback,
  assignment_date_col = NULL,
  discount_rate,
  conditions,
  current_period
) {
  # Faster execution using vectors and `tapply()` when data is small
  if (nrow(prior_data) < 30000) {
    compute_prior.fast(
      current_data = current_data,
      prior_data = prior_data,
      delayed_feedback = delayed_feedback,
      assignment_date_col = assignment_date_col,
      discount_rate = discount_rate,
      conditions = conditions,
      current_period = current_period
    )
  } else {
    UseMethod("compute_prior", current_data)
  }
}


#' @method compute_prior fast
#' @title
#' [compute_prior()] using lower level base `R` for efficiency when data size is small
#' @inheritParams compute_prior
#' @noRd
compute_prior.fast <- function(
  current_data,
  prior_data,
  delayed_feedback,
  assignment_date_col = NULL,
  discount_rate,
  conditions,
  current_period
) {
  if (delayed_feedback) {
    current_date <- max(current_data[[assignment_date_col]])
    known_success <- as.integer(
      current_date >= prior_data[["new_success_date"]] &
        !is.na(prior_data[["new_success_date"]])
    )
  } else {
    known_success <- prior_data[["mab_success"]]
  }
  weight <- discount_rate^(current_period -
    prior_data[["period_number"]])

  successes <- tapply(
    X = (prior_data[["known_success"]] * prior_data[["weight"]]),
    INDEX = prior_data[["mab_condition"]],
    FUN = sum
  )
  n <- tapply(
    X = prior_data[["weight"]],
    INDEX = prior_data[["mab_condition"]],
    FUN = sum
  )
  if (!identical(names(successes), names(n))) {
    n <- n[names(successes)]
  }

  prior_list <- list(
    mab_condition = names(successes),
    successes = successes,
    n = n
  ) |>
    finalize_prior_list(conditions = conditions)
  return(prior_list)
}

#----------------------------------------------------------------------------------
#' @method compute_prior data.frame
#' @title
#' [compute_prior()] for data.frames
#' @inheritParams compute_prior
#' @noRd

compute_prior.data.frame <- function(
  current_data,
  prior_data,
  delayed_feedback,
  assignment_date_col = NULL,
  discount_rate,
  conditions,
  current_period
) {
  if (delayed_feedback) {
    current_date <- max(current_data[[assignment_date_col]])

    prior_data[["known_success"]] <- as.integer(
      current_date >= prior_data[["new_success_date"]] &
        !is.na(prior_data[["new_success_date"]])
    )
  } else {
    prior_data[["known_success"]] <- prior_data[["mab_success"]]
  }
  prior_data[["weight"]] <- discount_rate^(current_period -
    prior_data[["period_number"]])

  prior_list <- prior_data |>
    dplyr::group_by(mab_condition) |>
    dplyr::summarize(
      successes = sum(known_success * weight, na.rm = TRUE),
      n = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) |>
    as.list() |>
    finalize_prior_list(conditions = conditions)

  return(prior_list)
}
#------------------------------------------------------------------------------

#' @method compute_prior data.table
#' @title
#' [compute_prior()] for data.tables
#' @inheritParams compute_prior
#' @noRd

compute_prior.data.table <- function(
  current_data,
  prior_data,
  delayed_feedback,
  assignment_date_col = NULL,
  discount_rate,
  conditions,
  current_period
) {
  if (delayed_feedback) {
    current_date <- max(current_data[[assignment_date_col]])

    prior_data[,
      known_success := as.integer(
        current_date >= new_success_date &
          !is.na(new_success_date)
      )
    ]
  } else {
    prior_data[, known_success := mab_success]
  }

  prior_data[, discount_period := current_period - period_number][,
    weight := discount_rate^discount_period
  ]
  prior_list <- prior_data[,
    .(
      successes = sum(known_success * weight, na.rm = TRUE),
      n = sum(weight, na.rm = TRUE)
    ),
    by = mab_condition
  ] |>
    as.list() |>
    finalize_prior_list(conditions = conditions)

  return(prior_list)
}
#---------------------------------------------------------------------------

#' Finalise Aggregated Prior Results
#' @name finalize_prior_list
#' @description Accepts the raw list output of an aggregation over `prior_data`
#' (from [compute_prior()]), names each vector by condition, fills any
#' conditions absent from the prior window with zeros, and sorts alphabetically.
#' @param prior_list Named list with elements `mab_condition`, `successes`, `n`,
#' produced by converting a summarized data.frame/data.table via [as.list()].
#' @param conditions Character vector of all treatment conditions in the trial.
#' @returns A named list with elements `successes`, `n`,
#' each a named numeric vector of length `length(conditions)`.
#' @keywords internal
finalize_prior_list <- function(prior_list, conditions) {
  nms <- prior_list[["mab_condition"]]
  prior_list[["mab_condition"]] <- NULL

  missing <- if (length(nms) != length(conditions)) {
    setdiff(conditions, nms)
  } else {
    NULL
  }

  ord <- order(c(nms, missing))

  prior_list <- lapply(
    prior_list,
    \(x) {
      names(x) <- nms
      if (!is.null(missing)) {
        x[missing] <- 0
      }
      x <- x[ord]
      return(x)
    }
  )

  return(prior_list)
}

#-------------------------------------------------------------------------------
#' Calculate Multi-Arm Bandit Decision Based on Algorithm
#' @description Calculates the best treatment for a given period using either a UCB1 or Thompson Sampling algorithm.
#' Thompson sampling is done using [bandit::best_binomial_bandit()] from
#' the \href{https://cran.r-project.org/package=bandit}{bandit}
#' package and UCB1 values are calculated using the well-defined formula that can be found
#' in \href{https://doi.org/10.1023/A:1013689704352}{Auer et al. (2002)}.
#'
#' @name compute_bandit
#' @inheritParams mab_loop
#' @param past_results A `tibble`/`data.table`` containing summary of prior periods, with
#' successes, number of observations, and success rates, which is created by [compute_prior()].
#' @param current_period Numeric value of length 1; current period of the adaptive trial simulation.
#'
#' @returns A list of length 2 containing:
#' \itemize{
#' \item `bandit`: Bandit object, either a named numeric vector of Thompson sampling probabilities UCB1 values.
#' \item `assignment_probabilities:` Named numeric vector with probabilities of being assigned to the given treatment, where `names(.)` are the treatments.
#' Adjusted for control augmentation}
#'
#' @details
#'
#' The Thompson `assignment_probabilities` are the same as the `bandit` vector except when
#' `control_augment` or `random_assign_prop` are greater than 0, as these arguments will alter the probabilities
#' of assignment.
#'
#' Thompson sampling is calculated using the \href{https://cran.r-project.org/package=bandit}{bandit}
#' package but the direct calculation can result in errors or overflow. If this occurs, a simulation based method
#' from the same package is used instead to estimate the posterior distribution.
#' If this occurs a warning will be presented. `ndraws` specifies the number of iterations for the
#' simulation based method, and the default value is 5000.
#'
#' The UCB1 algorithm only selects 1 treatment at each period, with no probability matching
#' so `assignment_probabilities` will always have 1 element equal to 1, and the rest equal to 0, unless
#' `control_augment` or `random_assign_prop` are greater than 0, which will alter the probabilities of assignment.
#' For example, if the original vector is `(0, 0, 1)`, and `control_augment` = 0.2,
#' the new vector is `(0.2, 0, 0.8)` assuming the first element is control. If instead the 3rd element
#' were the control group the resulting vector would not be changed because it already meets the
#' control group threshold.
#'
#'
#' @references
#'
#' Auer, Peter, Nicolò Cesa-Bianchi, and Paul Fischer. 2002.
#' "Finite-Time Analysis of the Multiarmed Bandit Problem." \emph{Machine Learning}
#' 47 (2): 235–56. \doi{10.1023/A:1013689704352}.
#'
#' Kuleshov, Volodymyr, and Doina Precup. 2014. "Algorithms for Multi-Armed Bandit Problems."
#' \emph{arXiv}. \doi{10.48550/arXiv.1402.6028}.
#'
#' Loecher, Thomas Lotze and Markus. 2022.
#' "Bandit: Functions for Simple a/B Split Test and Multi-Armed Bandit Analysis."
#' \url{https://cran.r-project.org/package=bandit}.
#'
#' Thompson, William R. 1933. "On the Likelihood That One Unknown Probability Exceeds Another in View of the Evidence of Two Samples."
#'  Biometrika 25 (3/4): 285–94. \doi{10.2307/2332286}
#' @keywords internal

compute_bandit <- function(
  past_results = NULL,
  algorithm,
  num_conditions,
  conditions,
  current_period,
  control_augment = 0,
  ndraws
) {
  bandit <- switch(
    algorithm,
    "thompson" = compute_bandit.thompson(
      past_results = past_results,
      conditions = conditions,
      current_period = current_period,
      ndraws = ndraws
    ),
    "ucb1" = compute_bandit.ucb1(
      past_results = past_results,
      conditions = conditions,
      num_conditions = num_conditions,
      current_period = current_period
    )
  )

  assignment_prob <- bandit[["assignment_prob"]]

  if (control_augment > 0) {
    ctrl <- names(conditions) == "control"
    if (assignment_prob[ctrl] < control_augment) {
      assignment_prob[ctrl] <- control_augment
      assignment_prob[!ctrl] <- (assignment_prob[!ctrl] /
        sum(assignment_prob[!ctrl])) *
        (1 - control_augment)
    }
  }
  if (!isTRUE(all.equal(sum(assignment_prob), 1))) {
    bandit[["assignment_prob"]] <- assignment_prob / sum(assignment_prob)
  }

  return(bandit)
}
#-------------------------------------------------------------------
#' @method compute_bandit thompson
#' @title Thompson sampling Algorithm
#' @inheritParams compute_bandit
#' @details
#' Thompson Sampling is calculated using the \href{https://cran.r-project.org/package=bandit}{bandit}
#' package but the direct calculation can fail. If this occurs, a simulation based method is used
#' instead to estimate the posterior distribution, and the user receives a warning.
#'
#' @returns A list containing 2 named vectors where `names()` correspond to treatments. Both vectors are the computed
#' Thompson Sampling probabilities.
#' @keywords internal

compute_bandit.thompson <- function(
  past_results,
  conditions,
  current_period,
  ndraws
) {
  bandit <- tryCatch(
    {
      ts <- bandit::best_binomial_bandit(
        x = past_results[["successes"]],
        n = past_results[["n"]],
        alpha = 1,
        beta = 1
      ) |>
        as.vector()
      if (bandit_invalid(ts)) {
        stop("Invalid Bandit")
      }
      return(ts)
    },
    error = function(e) {
      rlang::warn(c(
        "Thompson sampling calculation overflowed; simulation based posterior estimate was used instead",
        "i" = sprintf("Period: %d", current_period)
      ))
      ts <- bandit::best_binomial_bandit_sim(
        x = past_results[["successes"]],
        n = past_results[["n"]],
        alpha = 1,
        beta = 1,
        ndraws = ndraws
      ) |>
        as.vector()
      return(ts)
    }
  )
  names(bandit) <- names(past_results[["successes"]])

  if (bandit_invalid(bandit)) {
    rlang::abort(c(
      "Thompson sampling simulation failed",
      "x" = sprintf(
        "Most Recent Result: %s",
        paste0(bandit, collapse = ",")
      ),
      "i" = "Consider setting `ndraws` higher or reducing `prior_periods`."
    ))
  }

  return(list(bandit = bandit, assignment_prob = bandit))
}
#' @name bandit_invalid
#' @title Checks Validity of Thompson Sampling probabilities
#' @description Checks if the Thompson Sampling probabilities either sum arbitrarily close
#' to 0 or if any of them are NA, indicating the direct calculation failed or did not converge.
#' @param bandit a numeric vector of Thompson Sampling probabilities.
#' @returns Logical; TRUE if the vector is invalid, FALSE if valid
#' @keywords internal
bandit_invalid <- function(bandit) {
  return(any(is.na(bandit)) || isTRUE(all.equal(sum(bandit), 0)))
}
#-------------------------------------------------------------------
#' @method compute_bandit ucb1
#' @title UCB1 Sampling Algorithm
#' @description Calculates upper confidence bounds for each treatment arm
#' @inheritParams compute_bandit
#' @returns A list containing 2 named vectors where `names()` correspond to treatments. The first vector
#' is the computed UCB1 values, and the second is the corresponding assignment probabilities where the highest UCB1 is given
#' `1` and all else are `0`.
#' @keywords internal

compute_bandit.ucb1 <- function(
  past_results,
  num_conditions,
  conditions,
  current_period
) {
  correction <- 1e-10 ## Prevents Division by 0 when n = 0
  n_safe <- pmax(past_results[["n"]], correction)
  success_rates <- past_results[["successes"]] / n_safe
  ucb1 <- success_rates +
    sqrt((2 * log(current_period)) / n_safe)

  best <- names(ucb1)[which.max(ucb1)]
  assignment_probs <- stats::setNames(
    rep(0, length(ucb1)),
    names(ucb1)
  )
  assignment_probs[[best]] <- 1

  return(list(
    bandit = ucb1,
    assignment_prob = assignment_probs
  ))
}
#-------------------------------------------------------------------------------
#' Adaptively Assign Treatments in a Period
#' @description Assigns new treatments for an assignment wave based on the assignment probabilities provided from
#' [compute_bandit()], and the proportion of randomly assigned observations specified in `random_assign_prop`.
#' Assignments are made randomly with the given probabilities using [randomizr::block_ra()],
#' [randomizr::complete_ra()], [randomizr::cluster_ra()], or [randomizr::block_and_cluster_ra()]
#' depending on whether blocking and/or clustering are used.
#'
#' @name assign_treatments
#' @inheritParams run_mab
#' @inheritParams mab_from_rct
#' @param condition_col Column name of `current_data` which holds original treatment assignments.
#' @param cluster_col Column name of `current_data` which holds cluster assignments.
#' @param probs Named numeric vector; probability of assignment for each treatment condition.
#' @param random_probs Probabilities of assignment for the rows which are completely randomly assigned. Simply a vector
#' of `length(conditions)` with the same equal probability for all elements.
#' @inheritParams compute_prior
#' @returns Updated `tibble` or `data.table` with the new treatment conditions for each observation, and whether imputation is required.
#' If this treatment is different then from under the original experiment, then 'impute_req = 1`, and else is 0 for the observation.
#'
#' @details
#' The number of rows which are randomly assigned in each period is `random_assign_prop` multiplied by
#' the number of rows in the period. If this number is less than 1, then Bernoulli draws are made for each row
#' with probability `random_assign_prop` to determine if that row will be assigned randomly. Else, the number of random
#' rows is rounded to the nearest whole number, and then that many rows are selected to be assigned through
#' complete random assignment. The row selections are also random.
#'
#' Clustering introduces difficulties with `random_assign_prop` so a more advanced algorithm is used to determine assignment. When `random_rows < 1`,
#' Bernoulli draws are made for each cluster with probabilitiy `random_assign_prop`, so its possible for the number of rows to be assigned randomly is far
#' larger than the provided proportion if cluster sizes are imbalanced. When `random_rows > 1`, a random permutation of the clusters is made and then
#' clusters are selected for random assignment greedily until the cumulative count surpasses `random_rows`.
#' @seealso
#'* [randomizr::block_ra()]
#'* [randomizr::complete_ra()]
#'* [randomizr::cluster_ra()]
#'* [randomizr::block_and_cluster_ra()]
#' @keywords internal
assign_treatments <- function(
  current_data,
  probs,
  blocking = NULL,
  clustering = NULL,
  conditions,
  condition_col = NULL,
  cluster_col = NULL,
  random_assign_prop,
  random_probs = NULL,
  resimulation
) {
  rows <- nrow(current_data)
  random_rows <- rows * random_assign_prop

  rand_idx <- if (clustering && random_assign_prop > 0) {
    if (random_rows < 1) {
      clusters <- unique(current_data[[cluster_col]])
      rand_clusters <- clusters[as.logical(stats::rbinom(
        length(clusters),
        1,
        random_assign_prop
      ))]
      which(current_data[[cluster_col]] %in% rand_clusters)
    } else {
      clusters <- unique(current_data[[cluster_col]])
      cluster_sizes <- table(current_data[[cluster_col]])

      cluster_permutation <- sample(names(cluster_sizes))
      cumulative_counts <- cumsum(cluster_sizes[cluster_permutation])
      clusters_idx <- which(cumulative_counts >= random_rows)[1] # Take the first that is larger as last cluster

      which(
        current_data[[cluster_col]] %in%
          cluster_permutation[seq_len(clusters_idx)]
      )
    }
  } else {
    if (random_rows < 1) {
      which(as.logical(stats::rbinom(rows, 1, random_assign_prop)))
    } else {
      sample(
        x = rows,
        size = round(random_rows, 0),
        replace = FALSE
      )
    }
  }

  band_idx <- setdiff(seq_len(rows), rand_idx)

  assignment_type <- vector(
    mode = "character",
    length = nrow(current_data)
  )
  assignment_type[band_idx] <- "bandit"
  assignment_type[rand_idx] <- "random"

  if (data.table::is.data.table(current_data)) {
    assign_treatments.data.table(
      current_data = current_data,
      probs = probs,
      blocking = blocking,
      clustering = clustering,
      conditions = conditions,
      condition_col = condition_col,
      cluster_col = cluster_col,
      rand_idx = rand_idx,
      band_idx = band_idx,
      random_probs = random_probs,
      assignment_type = assignment_type,
      resimulation = resimulation
    )
  } else {
    assign_treatments.data.frame(
      current_data = current_data,
      probs = probs,
      blocking = blocking,
      clustering = clustering,
      conditions = conditions,
      condition_col = condition_col,
      cluster_col = cluster_col,
      rand_idx = rand_idx,
      band_idx = band_idx,
      random_probs = random_probs,
      assignment_type = assignment_type,
      resimulation = resimulation
    )
  }
}

#' Build `{randomizr}`` function and arguments
#' @name build_ra_args
#' @description Selects the appropriate `{randomizr}` function and constructs its argument list
#' based on whether blocking and/or clustering are requested.
#' @inheritParams compute_prior
#' @inheritParams assign_treatments
#' @param idx Integer vector of row indices to assign.
#' @param dt Logical. Whether `current_data` is a data.table.
#' @returns A list with `fn` (the randomizr function) and `args` (its arguments).
#' @keywords internal
build_ra_args <- function(
  idx,
  current_data,
  probs = NULL,
  conditions,
  blocking,
  clustering,
  cluster_col = NULL,
  dt
) {
  if (blocking && clustering) {
    list(
      fn = randomizr::block_and_cluster_ra,
      args = list(
        blocks = current_data[["block"]][idx],
        clusters = current_data[[cluster_col]][idx],
        prob_each = probs,
        conditions = conditions,
        check_inputs = TRUE
      )
    )
  } else if (blocking) {
    list(
      fn = randomizr::block_ra,
      args = list(
        blocks = current_data[["block"]][idx],
        prob_each = probs,
        conditions = conditions,
        check_inputs = TRUE
      )
    )
  } else if (clustering) {
    list(
      fn = randomizr::cluster_ra,
      args = list(
        clusters = current_data[[cluster_col]][idx],
        prob_each = probs,
        conditions = conditions,
        check_inputs = TRUE
      )
    )
  } else {
    list(
      fn = randomizr::complete_ra,
      args = list(
        N = length(idx),
        prob_each = probs,
        conditions = conditions,
        check_inputs = TRUE
      )
    )
  }
}

#' Assign Treatments to Bandit and Random Subsets
#' @name compute_assignments
#' @description Pre-allocates a character vector and fills treatment assignments
#' for bandit and randomly assigned subsets separately, using the appropriate
#' randomizr function built by [build_ra_args()].
#' @inheritParams compute_prior
#' @inheritParams assign_treatments
#' @param band_idx Integer vector of bandit-assigned row indices
#' @param rand_idx Integer vector of randomly-assigned row indices
#' @returns Character vector of length `nrow(current_data)` with treatment assignments
#' @keywords internal
compute_assignments <- function(
  current_data,
  band_idx,
  rand_idx,
  probs,
  random_probs = NULL,
  conditions,
  blocking,
  clustering,
  cluster_col = NULL
) {
  assignments <- vector("character", nrow(current_data))

  for (idx in list(band_idx, rand_idx)) {
    if (length(idx) == 0) {
      next
    }
    prob <- if (identical(idx, rand_idx)) random_probs else probs
    ra <- build_ra_args(
      idx = idx,
      current_data = current_data,
      probs = prob,
      conditions = conditions,
      blocking = blocking,
      clustering = clustering,
      cluster_col = cluster_col
    )
    assignments[idx] <- as.character(do.call(
      ra[["fn"]],
      ra[["args"]]
    ))
  }

  return(assignments)
}

#----------------------------------------------------------------------------------

#' @method assign_treatments data.frame
#' @title [assign_treatments()] for data.frames
#' @noRd
assign_treatments.data.frame <- function(
  current_data,
  probs,
  blocking,
  clustering,
  conditions,
  condition_col = NULL,
  cluster_col = NULL,
  rand_idx,
  band_idx,
  random_probs = NULL,
  assignment_type,
  resimulation
) {
  current_data[["assignment_type"]] <- assignment_type
  current_data[["mab_condition"]] <- compute_assignments(
    current_data = current_data,
    band_idx = band_idx,
    rand_idx = rand_idx,
    probs = probs,
    random_probs = random_probs,
    conditions = conditions,
    blocking = blocking,
    clustering = clustering,
    cluster_col = cluster_col
  )

  if (!resimulation) {
    current_data[["impute_req"]] <- as.integer(
      as.character(current_data[["mab_condition"]]) !=
        as.character(current_data[[condition_col]])
    )
  }
  return(current_data)
}

#' @method assign_treatments data.table
#' @title [assign_treatments()] for data.tables
#' @noRd
assign_treatments.data.table <- function(
  current_data,
  probs,
  blocking,
  clustering,
  conditions,
  condition_col = NULL,
  cluster_col = NULL,
  rand_idx,
  band_idx,
  random_probs = NULL,
  assignment_type,
  resimulation
) {
  current_data[, `:=`(
    assignment_type = assignment_type,
    mab_condition = compute_assignments(
      current_data = current_data,
      band_idx = band_idx,
      rand_idx = rand_idx,
      probs = probs,
      random_probs = random_probs,
      conditions = conditions,
      blocking = blocking,
      clustering = clustering,
      cluster_col = cluster_col
    )
  )]

  if (resimulation) {
    current_data[,
      impute_req := as.integer(
        as.character(mab_condition) != as.character(get(condition_col))
      )
    ]
  }
  return(invisible(current_data))
}

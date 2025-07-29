#' Gather Past Results for Given Assignment Period
#' @name get_past_results
#' @description Summarizes results of prior periods to use for the current Multi-Arm-Bandit assignment. This function
#' calculates the number of success under each treatment, the total number of observations assigned to each treatment,
#' and the success rate my treatment, to be used for UCB1 or Thompson Sampling.
#'
#' @inheritParams single_mab_simulation
#' @inheritParams create_prior
#' @inheritParams cols
#' @param current_data tibble/data.table with only observations from the current sampling period.
#' @param prior_data tibble/data.table with only the observations from the prior index.
#' @returns A tibble/data.table containing the number of successes, and number of people for each
#' treatment condition.
#'
#' @details
#' When `perfect_assignment` is FALSE, the maximum value from the specified
#' `assignment_date_col` in the current data is taken as the last possible date
#' the researchers conducting the experiment could have learned about a treatment outcome.
#' All successes that occur past this date are masked and treated as failures for the purposes
#' of assigning this treatments periods, as it simulates the researchers not having
#' received that information yet.
#'
#' @seealso
#' *[run_mab_trial()]
#' *[single_mab_simulation()]
#' *[get_bandit()]
#' @keywords internal
#'
get_past_results <- function(current_data, prior_data, perfect_assignment, assignment_date_col = NULL,
                             conditions) {
  base::UseMethod("get_past_results", current_data)
}

#----------------------------------------------------------------------------------
#' @method get_past_results data.frame
#' @title
#' [get_past_results()] for data.frames
#' @inheritParams get_past_results
#' @noRd


get_past_results.data.frame <- function(current_data, prior_data, perfect_assignment, assignment_date_col = NULL,
                                        conditions) {
  if (!perfect_assignment) {
    current_date <- base::max(current_data[[assignment_date_col$name]])



    prior_data$known_success <- base::ifelse(
      current_date >= prior_data[["new_success_date"]] & !base::is.na(prior_data[["new_success_date"]]),
      1, 0
    )
  } else {
    prior_data$known_success <- prior_data$mab_success
  }

  prior_data <- prior_data |>
    dplyr::group_by(mab_condition) |>
    dplyr::summarize(
      successes = base::sum(known_success, na.rm = TRUE),
      success_rate = base::mean(known_success, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::ungroup()

  if (base::nrow(prior_data) != base::length(conditions)) {
    conditions_add <- base::setdiff(conditions, prior_data$mab_condition)

    replace <- tibble::tibble(
      mab_condition = conditions_add, successes = 0,
      success_rate = 0, n = 0
    )

    prior_data <- dplyr::bind_rows(prior_data, replace)
    prior_data <- prior_data[order(prior_data$mab_condition), ]
  }
  return(prior_data)
}
#------------------------------------------------------------------------------

#' @method get_past_results data.table
#' @title
#' [get_past_results()] for data.tables
#' @inheritParams get_past_results
#' @noRd


get_past_results.data.table <- function(current_data,
                                        perfect_assignment, assignment_date_col = NULL,
                                        conditions, prior_data) {
  if (!perfect_assignment) {
    current_date <- base::max(current_data[[assignment_date_col$name]])

    prior_data[, known_success := data.table::fifelse(
      current_date >= new_success_date &
        !is.na(new_success_date), 1, 0
    )]
  } else if (perfect_assignment) {
    prior_data[, known_success := mab_success]
  } else {
    rlang::abort("Specify Logical for `perfect_assignment`")
  }

  past_results <- prior_data[, .(
    successes = base::sum(known_success, na.rm = TRUE),
    success_rate = base::mean(known_success, na.rm = TRUE),
    n = .N
  ), by = mab_condition]



  if (base::nrow(past_results) != base::length(conditions)) {
    conditions_add <- base::setdiff(conditions, past_results$mab_condition)
    replace <- data.table::data.table(
      mab_condition = conditions_add, successes = 0,
      success_rate = 0, n = 0
    )

    past_results <- data.table::rbindlist(list(past_results, replace),
      use.names = TRUE
    )

    data.table::setorder(past_results, mab_condition)
  }
  return(invisible(past_results))
}

#-------------------------------------------------------------------------------
#' Calculate Multi-Arm Bandit Decision Based on Algorithm
#' @description Calculates the best treatment for a given period using either a UCB1 or Thompson Sampling Algorithm.
#' Thompson Sampling is done using [bandit::best_binomial_bandit()] from the \href{https://cran.r-project.org/package=bandit}{bandit}
#' package and UCB1 statistic are calculated using the a well-defined formula that can be found
#' in \href{https://doi.org/10.48550/arXiv.1402.6028}{(Kuleshov and Precup 2014)}.
#'
#' @name get_bandit
#'
#' @inheritParams single_mab_simulation
#' @param past_results tibble/data.table containing summary of prior periods, with
#' successes, number of observations, and success rates, which is created by [get_past_results()].
#' @param current_period Numeric scalar; current period of the adaptive trial simulation.
#'
#' @returns A list of length 2 containing:
#' \itemize{
#' \item `bandit`: Bandit object, either a named numeric vector of Thompson Probabilities or a
#' tibble/data.table of UCB1 statistics.
#' \item `assignment_probabilities:` Named numeric vector with a value for each condition
#' containing the probability of being assigned that treatment.}
#'
#' @details
#' The Thompson  `assignment_probabilities` is the same as the `bandit` except
#' for that that `assignment_probabilities` are forced to sum exactly to 1 (not 0.99999),
#' and the `assignment_probabilities` takes into account the control augmentation if used.
#'
#' For UCB1, the `assignment_probabilities` will always be of the have all 0
#' components expect for one 1, because the UCB1 algorithm chooses only 1
#' option at each period, based on the highest UCB1 statistic. When
#' control augmentation is used, the probabilities will reflect that as well
#' so it may be a vector like (0.2, 0, 0.8), if `control_augmentation` = 0.2.
#'
#'
#' @references
#' Kuleshov, Volodymyr, and Doina Precup. 2014. “Algorithms for Multi-Armed Bandit Problems.”
#' \emph{arXiv}. \doi{10.48550/arXiv.1402.6028}.
#'
#' #' Loecher, Thomas Lotze and Markus. 2022.
#' “Bandit: Functions for Simple a/B Split Test and Multi-Armed Bandit Analysis.”
#' \url{https://cran.r-project.org/package=bandit}.
#' @keywords internal


get_bandit <- function(past_results, algorithm, conditions, current_period, control_augment = 0, ndraws) {
  bandit <- switch(algorithm,
    "Thompson" = get_bandit.Thompson(
      past_results = past_results, conditions = conditions, current_period =
        current_period, ndraws = ndraws
    ),
    "UCB1" = get_bandit.UCB1(past_results = past_results, conditions = conditions, current_period = current_period),
    rlang::abort("Invalid `algorithm`. Valid Algorithms: 'Thomspon', 'UCB1'")
  )

  assignment_prob <- bandit[["assignment_prob"]]

  if (control_augment > 0) {
    ctrl <- base::which(base::names(conditions) == "Control")

    if (assignment_prob[ctrl] < control_augment) {
      assignment_prob[ctrl] <- control_augment
      assignment_prob[-ctrl] <-
        (assignment_prob[-ctrl] / sum(assignment_prob[-ctrl])) * (1 - control_augment)
    }
  }
  if (!isTRUE(all.equal(sum(assignment_prob), 1))) {
    assignment_prob <- assignment_prob / sum(assignment_prob)
  }
  bandit[["assignment_prob"]] <- assignment_prob

  return(bandit)
}
#-------------------------------------------------------------------
#' @method get_bandit Thompson
#' @title Thompson Sampling Algorithm
#' @inheritParams get_bandit
#' @details
#' Thompson Sampling is calculated using the \href{https://cran.r-project.org/package=bandit}{bandit}
#' package using R's integrate function which can overflow. If overflow errors occur, the Thompson Sampling
#' probabilities will be calculated by simulating draws from the posterior instead of direct calculations.
#'
#' @returns Named Numeric Vector of Posterior Probabilities
#' @keywords internal

get_bandit.Thompson <- function(past_results, conditions, current_period, ndraws) {
  bandit <- tryCatch(
    {
      result <- rlang::set_names(
        as.vector(bandit::best_binomial_bandit(
          x = past_results$successes,
          n = past_results$n,
          alpha = 1,
          beta = 1
        )), conditions
      )
      if (bandit_invalid(result)) {
        stop("Invalid Bandit")
      }
      result
    },
    error = function(e) {
      rlang::warn(c(
        "Thompson Sampling calculation overflowed; simulation based posterior estimate was used instead",
        "i" = sprintf("Period: %d", current_period)
      ))
      result <- rlang::set_names(
        as.vector(bandit::best_binomial_bandit_sim(
          x = past_results$successes,
          n = past_results$n,
          alpha = 1,
          beta = 1,
          ndraws = ndraws
        )), conditions
      )

      result
    }
  )

  if (bandit_invalid(bandit)) {
    rlang::abort(c("Thompson Sampling simulation overflowed",
      "x" = paste0("Most Recent Result:", paste0(bandit, collapse = " ")),
      "i" = "Consider setting `ndraws` higher or reducing `prior_periods`."
    ))
  }


  return(list(bandit = bandit, assignment_prob = bandit))
}
#' @name bandit_invalid
#' @title Checks Validity of Thompson Probabilities
#' @description Checks if the Thompson Probabilities either sum arbitrarily close
#' to 0 or if any of them are NA, signs that overflow occurred
#' @param bandit a numeric vector of Thompson Probabilities passed from
#' [get_bandit.Thompson()].
#' @returns Logical; TRUE if the vector is invalid, FALSE if valid
#' @keywords internal
bandit_invalid <- function(bandit) {
  return(isTRUE(all.equal(base::sum(bandit), 0)) | any(is.na(bandit)))
}
#-------------------------------------------------------------------
#' @method get_bandit UCB1
#' @title UCB1 Sampling Algorithm
#' @description
#' Utilizes the well established UCB1 formula to calculate Upper
#' Confidence Bounds for each treatment arm.
#'
#' @inheritParams get_bandit
#' @returns tibble/data.table containing UCB1 and Success Rate for each condition
#' @keywords internal

get_bandit.UCB1 <- function(past_results, conditions, current_period) {
  correction <- 1e-10 ## Prevents Division by 0 when n = 0

  if (inherits(past_results, "data.table")) {
    past_results[, ucb := success_rate + base::sqrt(
      (2 * base::log(current_period - 1)) / (n + correction)
    )]

    best_condition <- base::as.character(past_results[which.max(ucb), mab_condition])
  } else {
    past_results$ucb <- past_results$success_rate +
      base::sqrt((2 * base::log(current_period - 1)) / (past_results$n + correction))

    best_condition <- past_results$mab_condition[base::which.max(past_results$ucb)]
  }

  assignment_probs <- rlang::set_names(rep_len(0, length.out = length(conditions)), conditions)

  assignment_probs[[best_condition]] <- 1

  return(invisible(list(bandit = past_results, assignment_prob = assignment_probs)))
}
#-------------------------------------------------------------------------------
#' Adaptively Assign Treatments in a Period
#' @description Assigns new treatments for an assignment wave based on the assignment probabilities provided from
#' [get_bandit()], and the proportion of randomly assigned observations from specified in `random_assign_prop`
#' Probabilities are passed to [randomizr::block_and_cluster_ra()] or [randomizr::cluster_ra()] from the
#' \href{https://cran.r-project.org/package=randomizr}{randomizr} package for random assignment.
#' @name assign_treatments
#' @inheritParams single_mab_simulation
#' @inheritParams cols
#' @param probs Named Numeric Vector; Probability of Assignment for each treatment condition.
#' @inheritParams get_past_results
#' @returns Updated tibble/data.table with the new treatment conditions for each observation. If this treatment is different
#' then from under the original experiment, they are labelled as imputation required.
#'
#' @details
#' The unique identifier for each row is used as the cluster in the for the
#' randomizr package functions to ensure that each observation is only
#' treated once, and is returned in the same order as provided.
#'
#'
#' @seealso
#'* [randomizr::block_and_cluster_ra()]
#'* [randomizr::cluster_ra()]
#' @keywords internal

assign_treatments <- function(current_data, probs, blocking = NULL,
                              algorithm, id_col, conditions, condition_col,
                              success_col, random_assign_prop) {
  base::UseMethod("assign_treatments", current_data)
}
#' @method assign_treatments data.frame
#' @title [assign_treatments()] for data.frames
#' @noRd
assign_treatments.data.frame <- function(current_data, probs, blocking = NULL,
                                         algorithm, id_col, conditions, condition_col,
                                         success_col, random_assign_prop) {
  rows <- base::nrow(current_data)
  random_rows <- base::round(rows * random_assign_prop, 0)
  if (random_rows == 0 && random_assign_prop > 0) {
    rand_idx <- base::which(base::as.logical(stats::rbinom(rows, 1, random_assign_prop)))
  } else {
    rand_idx <- base::sample(x = rows, size = random_rows, replace = FALSE)
  }

  num_conditions <- base::length(conditions)
  random_probs <- base::rep_len(1 / num_conditions, length.out = num_conditions)
  band_idx <- base::setdiff(seq_len(rows), rand_idx)

  ids <- current_data[[id_col$name]]
  bandit_clusters <- ids[band_idx]
  random_clusters <- ids[rand_idx]

  current_data$assignment_type[band_idx] <- "bandit"
  current_data$assignment_type[rand_idx] <- "random"

  if (blocking) {
    bandit_blocks <- current_data$block[band_idx]
    random_blocks <- current_data$block[rand_idx]
    if (length(rand_idx) > 0) {
      current_data$mab_condition[rand_idx] <- base::as.character(randomizr::block_and_cluster_ra(
        clusters = random_clusters,
        blocks = random_blocks,
        prob_each = random_probs,
        conditions = conditions,
        check_inputs = FALSE
      ))
    }
    if (base::length(band_idx) > 0) {
      current_data$mab_condition[band_idx] <- base::as.character(randomizr::block_and_cluster_ra(
        clusters = bandit_clusters,
        blocks = bandit_blocks,
        prob_each = probs,
        conditions = conditions,
        check_inputs = FALSE
      ))
    }
  } else {
    if (base::length(rand_idx) > 0) {
      current_data$mab_condition[rand_idx] <- base::as.character(randomizr::cluster_ra(
        clusters = random_clusters,
        prob_each = random_probs,
        conditions = conditions,
        check_inputs = FALSE
      ))
    }
    if (base::length(band_idx) > 0) {
      current_data$mab_condition[band_idx] <- base::as.character(randomizr::cluster_ra(
        clusters = bandit_clusters,
        prob_each = probs,
        conditions = conditions,
        check_inputs = FALSE
      ))
    }
  }
  current_data$impute_req <- base::ifelse(
    base::as.character(current_data$mab_condition) !=
      base::as.character(current_data[[condition_col$name]]), 1, 0
  )
  return(current_data)
}

#' @method assign_treatments data.table
#' @title [assign_treatments()] for data.tables
#' @noRd
assign_treatments.data.table <- function(current_data, probs, blocking = NULL,
                                         algorithm, id_col, conditions, condition_col,
                                         success_col, random_assign_prop) {
  rows <- base::nrow(current_data)
  random_rows <- base::round(rows * random_assign_prop, 0)
  if (random_rows == 0 && random_assign_prop > 0) {
    rand_idx <- base::which(base::as.logical(stats::rbinom(rows, 1, random_assign_prop)))
  } else {
    rand_idx <- base::sample(x = rows, size = random_rows, replace = FALSE)
  }

  num_conditions <- base::length(conditions)
  random_probs <- base::rep_len(1 / num_conditions, length.out = num_conditions)
  band_idx <- base::setdiff(seq_len(rows), rand_idx)

  ids <- current_data[, base::get(id_col$name)]
  bandit_clusters <- ids[band_idx]
  random_clusters <- ids[rand_idx]

  current_data[band_idx, assignment_type := "bandit"]
  current_data[rand_idx, assignment_type := "random"]

  if (blocking) {
    bandit_blocks <- current_data[band_idx, block]
    random_blocks <- current_data[rand_idx, block]

    if (length(rand_idx) > 0) {
      current_data[rand_idx, mab_condition := base::as.character(randomizr::block_and_cluster_ra(
        clusters = random_clusters,
        blocks = random_blocks,
        prob_each = random_probs,
        conditions = conditions,
        check_inputs = FALSE
      ))]
    }
    if (base::length(band_idx) > 0) {
      current_data[band_idx, mab_condition := base::as.character(randomizr::block_and_cluster_ra(
        clusters = bandit_clusters,
        blocks = bandit_blocks,
        prob_each = probs,
        conditions = conditions,
        check_inputs = FALSE
      ))]
    }
  } else {
    if (base::length(rand_idx) > 0) {
      current_data[rand_idx, mab_condition := base::as.character(randomizr::cluster_ra(
        clusters = random_clusters,
        prob_each = random_probs,
        conditions = conditions,
        check_inputs = FALSE
      ))]
    }
    if (base::length(band_idx) > 0) {
      current_data[band_idx, mab_condition := base::as.character(randomizr::cluster_ra(
        clusters = bandit_clusters,
        prob_each = probs,
        conditions = conditions,
        check_inputs = FALSE
      ))]
    }
  }
  current_data[, impute_req := data.table::fifelse(
    base::as.character(mab_condition) != base::as.character(base::get(condition_col$name)), 1, 0
  )]

  return(invisible(current_data))
}

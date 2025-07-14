#' Gather Past Results for Given Assignment Period
#' @name get_past_results
#' @description Summarizes results of prior periods to use for the current Multi-Arm-Bandit assignment.
#'
#' @inheritParams single_mab_simulation
#' @inheritParams create_prior
#' @inheritParams cols
#' @param current_data Data with only observations from the current sampling period.
#' @param prior_data Data with only the observations from the prior index.
#' @returns A data.frame, containing the number of successes, and number of people for each
#' treatment condition.
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
#'
#' @name get_bandit
#'
#' @inheritParams single_mab_simulation
#' @param past_results `data` object containing summary of prior periods.
#' Created by [get_past_results()].
#' @param current_period Numeric scalar; current period of the adaptive trial simulation.
#'
#'
#' @returns The bandit object for the given period.
#'
#' @seealso
#'* [run_mab_trial()]
#'* [get_past_results()]
#' @keywords internal


get_bandit <- function(past_results, algorithm, conditions, current_period = NULL, control_augment = 0) {
  bandit <- switch(algorithm,
    "Thompson" = get_bandit.Thompson(past_results = past_results, conditions = conditions, iterator = 0),
    "UCB1" = get_bandit.UCB1(past_results = past_results, conditions = conditions, current_period = current_period),
    rlang::abort("Invalid `algorithm`. Valid Algorithms: 'Thomspon', 'UCB1'")
  )

  if (control_augment > 0) {
    bandit[[2]] <- augment_prob(
      assignment_probs = bandit[[2]], control_augment = control_augment,
      conditions = conditions, algorithm = algorithm
    )
  }

  if (base::sum(bandit[[2]]) != 1) {
    bandit[[2]] <- bandit[[2]] / base::sum(bandit[[2]])
  }

  return(bandit)
}
#-------------------------------------------------------------------
#' @method get_bandit Thompson
#' @title Thompson Sampling Algorithm
#' @param iterator counter variable; keeps track of recursive calls to prevent infinite recursion.
#' @inheritParams get_bandit
#' @returns Named Numeric Vector of Posterior Probabilities
#' @keywords internal

get_bandit.Thompson <- function(past_results, conditions, iterator) {
  bandit <- rlang::set_names(bandit::best_binomial_bandit(
    x = past_results$successes,
    n = past_results$n,
    alpha = 1,
    beta = 1
  ), conditions)

  if (base::sum(bandit) == 0 & iterator < 50) {
    # When best_binomial_bandit fails due to numerical instability,
    # this preserves proportions and reruns the process
    past_results$successes <- (past_results$successes / 2)
    past_results$n <- (past_results$n / 2)

    bandit <- get_bandit.Thompson(past_results = past_results, conditions = conditions, iterator = iterator + 1)[[1]]
  }


  return(list(bandit, assignment_prob = bandit))
}
#-------------------------------------------------------------------
#' @method get_bandit UCB1
#' @title UCB1 Sampling Algorithm
#' @inheritParams get_bandit
#' @returns Data.frame containing UCB and Success Rate for each condition
#' @keywords internal

get_bandit.UCB1 <- function(past_results, conditions, current_period) {
  correction <- 1e-10

  if (inherits(past_results, "data.table")) {
    past_results[, ucb := success_rate + base::sqrt(
      (2 * base::log(current_period - 1)) / (n + correction)
    )]

    best_condition <- base::as.character(past_results[order(ucb)][1, mab_condition])
  } else {
    past_results <- past_results |>
      dplyr::mutate(
        ucb = success_rate + base::sqrt(
          (2 * base::log(current_period - 1)) / (n + correction)
        )
      )
    best_condition <- past_results |>
      dplyr::slice_max(order_by = ucb, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::pull(mab_condition)
  }

  assignment_probs <- rlang::set_names(rep(0, length(conditions)), conditions)

  assignment_probs[[best_condition]] <- 1

  return(invisible(list(past_results, assignment_probs)))
}

#' @name augment_prob
#' @title Control Augmentation for Treatment Assignment
#' @description
#' Adjusts Probabilities of Assignment to match a control augmentation framework.
#' If probability threshold is not meant, values are adjusted uniformly to augment the control probability.
#' @inheritParams single_mab_simulation
#' @param assignment_probs Named numeric vector; contains probabilities of
#' assignment with the control condition named "Control".
#' @returns Named numeric vector with updated probabilities
#' @keywords internal

augment_prob <- function(assignment_probs, control_augment, conditions, algorithm) {
  assignment_probs <- switch(algorithm,
    "Thompson" = augment_prob.Thompson(
      assignment_probs = assignment_probs,
      control_augment = control_augment,
      conditions = conditions
    ),
    "UCB1" = augment_prob.UCB1(
      assignment_probs = assignment_probs,
      control_augment = control_augment,
      conditions = conditions
    ),
    rlang::abort("Invalid `algorithm`. Valid Algorithms: 'Thomspon', 'UCB1'")
  )
}
#' @method augment_prob Thompson
#' @title Augment Prob For Thompson Sampling
#' @inheritParams augment_prob
#' @keywords internal
augment_prob.Thompson <- function(assignment_probs, control_augment, conditions) {
  ctrl <- base::names(conditions) == "Control"

  if (assignment_probs[ctrl] < control_augment) {
    diff <- control_augment - assignment_probs[ctrl]

    sub_from <- diff / base::length(conditions)

    assignment_probs[ctrl] <- assignment_probs[ctrl] + diff

    assignment_probs[!ctrl] <- assignment_probs[!ctrl] - sub_from
  }
  if (any(assignment_probs < 0)) {
    assignment_probs <- fix_negatives(
      assignment_probs = assignment_probs,
      conditions = conditions
    )
  }

  return(assignment_probs)
}

#' @method augment_prob UCB1
#' @title Augment Prob For UCB1
#' @inheritParams augment_prob
#' @keywords internal
augment_prob.UCB1 <- function(assignment_probs, control_augment, conditions) {
  ctrl <- base::names(conditions) == "Control"
  selected <- assignment_probs == 1

  if (assignment_probs[ctrl] < control_augment) {
    diff <- control_augment - assignment_probs[ctrl]
    assignment_probs[ctrl] <- assignment_probs[ctrl] + diff
    assignment_probs[selected] <- assignment_probs[selected] - diff
  }

  return(assignment_probs)
}



#' @title Ensure Non-Zero Probabilities of Assignment
#' @description Redistributes Probabilities when control augmentation produces negatives
#' @name fix_negatives
#' @param assignment_probs Named Numeric Vector; Containing probabilities of treatment assignment
#' @param iter, iteration tracker, stops function if it reaches the limit of
#' @inheritParams single_mab_simulation
#' @returns Named Numeric Vector; Containing probabilities of treatment assignment, all positive.
#' @keywords internal

fix_negatives <- function(assignment_probs, conditions, iter = 1) {
  negatives <- assignment_probs < 0
  take_from <- assignment_probs > 0 & base::names(conditions) != "Control"

  amount_to_make_up <- sum(assignment_probs[negatives])

  assignment_probs[negatives] <- assignment_probs[negatives] - assignment_probs[negatives]

  sub_from <- amount_to_make_up / base::length(assignment_probs[take_from])

  assignment_probs[take_from] <- assignment_probs[take_from] - sub_from

  # Recursive Process that ends once an acceptable degree of precision is met

  if ((-1 * sum(assignment_probs[assignment_probs < 0]) > 1e-10) && iter < 50) {
    assignment_probs <- fix_negatives(
      assignment_probs = assignment_probs, conditions = conditions,
      iter = iter + 1
    )
  } else {
    assignment_probs[assignment_probs < 0] <- 1e-10
  }

  return(assignment_probs)
}
#-------------------------------------------------------------------------------
#' Adaptively Assign Treatments in a Period
#'
#' @description Assigns new treatments for an assignment wave based on the assignment probabilities provided.
#' Probabilities passed to [randomizr::block_and_cluster_ra()] or [randomizr::cluster_ra()] for random assignment.
#'
#' @name assign_treatments
#'
#' @inheritParams single_mab_simulation
#' @inheritParams create_prior
#' @inheritParams cols
#' @param probs Named Numeric Vector; Probability of Assignment for each treatment condition.
#' @inheritParams get_past_results
#'
#' @returns Updated `data` object with the new treatment conditions. If this treatment is different
#' then from under the original experiment, they are labelled as imputation required.
#'
#' @seealso
#'* [run_mab_trial()]
#'* [randomizr::block_and_cluster_ra()]
#'* [randomizr::cluster_ra()]
#' @keywords internal


assign_treatments <- function(current_data, probs, blocking = NULL,
                              algorithm, id_col, conditions, condition_col,
                              success_col) {
  # Performing Randomized Treatment Assignment
  if (blocking) {
    blocks <- current_data$block
  }
  clusters <- current_data[[id_col$name]]

  if (blocking) {
    new_treatments <- randomizr::block_and_cluster_ra(
      clusters = clusters,
      blocks = blocks,
      prob_each = probs,
      conditions = conditions,
      check_inputs = FALSE
    )
  } else if (!blocking) {
    new_treatments <- randomizr::cluster_ra(
      clusters = clusters,
      prob_each = probs,
      conditions = conditions,
      check_inputs = FALSE
    )
  } else {
    rlang::abort("Invalid: Specify TRUE or FALSE for blocking")
  }


  if (inherits(current_data, "data.table")) {
    current_data[, mab_condition := new_treatments][
      , impute_req := data.table::fifelse(
        base::as.character(mab_condition) != base::as.character(base::get(condition_col$name)),
        1, 0
      )
    ]
    return(invisible(current_data))
  } else {
    current_data$mab_condition <- new_treatments
    current_data$impute_req <- base::ifelse(
      base::as.character(current_data$mab_condition) !=
        base::as.character(current_data[[condition_col$name]]), 1, 0
    )

    return(current_data)
  }
}

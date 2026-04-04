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
#' where `rownames(p)` are the treatment. If there is a control condition, specify its rowname as `"Control"`
#' labels, and `colnames(p)` are the cluster or block labels, e.g.
#'       `matrix(c(0.5, 0.3, 0.5, 0.6), nrow = 2, ncol = 2, dimnames(list(c("Control", "T1"), c("B1", "B2"))))`.
#'       Probabilities are accessed as `p[treatment, block]`.
#' With blocks and clusters utilize the clusters for the columns because clusters are fully nested in blocks.
#' For no clusters or blocks simply use a matrix with 1 column.
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
#' during the trial, so outcomes are imperfectly observed. Only used when`dates_of_assignment` is also supplied. Dates can be generated even when `delayed_feedback == FALSE`,
#' but they will not be used.
#' Default `NULL`. Other optional arguments CANNOT share names as arguments in [furrr::furrr_options()]
#' @param algorithm Assignment algorithm, determines how probabilities of assignment
#' are updated each period. Either `"thompson"` for Thompson Sampling, `"ucb1"` for
#' the UCB1 algorithm, or `"static"` for uniform, non-adaptive assignment. Not case sensitive.
#' @param period_sizes Numeric vector of `length(t)`, with the specific number of units to be assigned in each period. Used when it is required to assign different numbers of units
#' to treatment across the periods of the trial.
#' @param ... Additional named arguments forwarded to `time_model` and [furrr::furrr_options()].
#' @inheritParams mab_from_rct
#'
#' @returns Depends on ` r` value if ` r = 1`, an S3 `single_param_mab` class object, and if ` r > 1`, an
#' S3 `muti_param_mab`, with the following:
#' \itemize{
#' \item `new_data`: `tibble` or `data.table` containing the new treatment assignments and outcomes under the simulation.
#' If ` r >1` and `keep_data = TRUE`, the tables from each trial are nested inside.
#' \item `bandits`: A list with 3 elements:
#' \itemize{
#' \item `statistic`: Thompson Sampling or UCB1 statistics computed for each treatment at each period of each trial.
#' \item `assignment_prob`: Assignment probabilities for each treatment at each period of each trial.
#' \item `assignment_quant`: Assignment quantities for each treatment in each trial.
#' }
#' \item `estimates`: A list with 2 elements:
#' \itemize{
#' \item `point`: A `tibble` or `data.table` containing point estimates, and variances for the AIPW, IPW, and Sample estimators
#' for each treatment in each trial. IPW also includes a joint-F statistic, and degrees of freedom
#' \item `vcov`: Variance covariance matrix of the IPW regression for each trial.
#' }
#' \item `config`: Configuration list of 3 elements:
#' \itemize{
#' \item `args`: List of arguments passed to [simulate_mab()].
#' \item `call`: The original call to [simulate_mab()].
#' \item `parallel`: The [furrr::furrr_options()] object used for parallelization.
#' }
#' }
#' @details
#' When blocking and/or clustering are specified, these assignments will be randomly pregenerated before the start of the adaptive sequential assignment. These arguments allow simulating a trial
#' when there may be hetergenous outcomes across a treatment block or treatment cluster, so different assignment probabilities can be provided for the same treatment, depending on the block and/or cluster
#' of a unit.
#'
#' Clustering is challenging under an adaptive trial, because then the probabilities of assignment being adaptive can have little impact on the new assignments, given that an early treatment assignment to a cluster
#' must remain the same across the whole trial. As such this function assumes clusters do not persist across periods, so are all respecitvely assigned at the same time. If a design is provided, as such periods are
#' too small for the clusters to fit in a period, its possible for assignment to vary within the same cluster in the experiment.
#' @export

simulate_mab <- function(
  n,
  t = n,
  p,
  algorithm,
  blocks = NULL,
  clusters = NULL,
  control_augment = 0,
  random_assign_prop = 0,
  delayed_feedback = FALSE,
  assignment_dates = NULL,
  time_model = NULL,
  period_sizes = NULL,
  prior_periods = NULL,
  discount_rate = 1,
  dt = FALSE,
  ndraws = 5000,
  r = 1,
  keep_data = FALSE,
  check_args = TRUE,
  verbose = FALSE,
  ...
) {
  cl <- match.call()
  args <- mget(methods::formalArgs(simulate_mab))
  algorithm <- tolower(algorithm)
  if (check_args) {
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
      delayed_feedback = delayed_feedback,
      time_model = time_model,
      period_sizes = period_sizes,
      prior_periods = prior_periods,
      discount_rate = discount_rate,
      dt = dt,
      ndraws = ndraws,
      r = r,
      keep_data = keep_data,
      verbose = verbose
    )
  }

  other_args <- split_args(..., time_model = time_model)
  period_idxs <- generate_period_idx(n = n, t = t, period_sizes = period_sizes)
  assignment_dates <- generate_assignment_dates(
    n = n,
    assignment_dates = assignment_dates
  )
  blocking <- !is.null(blocks)
  clustering <- !is.null(clusters)
  resimulation <- FALSE
  simulate_dates <- is.function(time_model) && !is.null(assignment_dates)
  rownames(p) <- tolower(rownames(p))
  conditions <- sort(rownames(p))
  names(conditions) <- ifelse(conditions == "control", "control", "treatment")

  p <- p[conditions, , drop = FALSE]

  equal_probs <- stats::setNames(rep(1 / nrow(p), nrow(p)), conditions)

  if (!"control" %in% names(conditions) && control_augment > 0) {
    rlang::abort(c(
      "a Control group must be specified when `control_augment` > 0",
      "x" = sprintf(
        "Treatment conditions specified: %s",
        paste(conditions, sep = ", ")
      ),
      "x" = paste0("Control Augment: ", control_augment)
    ))
  }

  col_names <- list(
    cluster_col = "cluster",
    assignment_date_col = "assignment_date",
    success_date_col = "success_date"
  )
  furrr_opt <- do.call(
    furrr::furrr_options,
    c(list(seed = TRUE), other_args$furrr_args)
  )
  verbose_log(verbose, "Starting Simulations")
  if (r == 1) {
    data <- prep_sim_data(
      n = n,
      p = p,
      blocks = blocks,
      clusters = clusters,
      blocking = blocking,
      clustering = clustering,
      period_idxs = period_idxs,
      conditions = conditions,
      equal_probs = equal_probs,
      assignment_dates = assignment_dates,
      simulate_dates = simulate_dates,
      time_model = time_model,
      time_model_args = time_model_args,
      dt = dt
    )
    results <- run_mab(
      data = data,
      sim_type = "param",
      p = p,
      algorithm = algorithm,
      control_augment = control_augment,
      random_assign_prop = random_assign_prop,
      prior_periods = prior_periods,
      discount_rate = discount_rate,
      simulate_dates = simulate_dates,
      delayed_feedback = delayed_feedback,
      conditions = conditions,
      blocking = blocking,
      clustering = clustering,
      col_names = col_names,
      verbose = verbose,
      ndraws = ndraws,
      starts = period_idxs[["start_idxs"]],
      ends = period_idxs[["end_idxs"]],
      keep_data = keep_data,
      r = r,
      time_model = time_model,
      time_model_args = other_args$time_model_args
    )
  } else if (r > 1) {
    mabs <- furrr::future_map(
      seq_len(r),
      \(.) {
        data <- prep_sim_data(
          n = n,
          p = p,
          blocks = blocks,
          clusters = clusters,
          blocking = blocking,
          clustering = clustering,
          period_idxs = period_idxs,
          conditions = conditions,
          simulate_dates = simulate_dates,
          equal_probs = equal_probs,
          assignment_dates = assignment_dates,
          time_model = time_model,
          time_model_args = other_args[["time_model_args"]],
          dt = dt
        )
        run_mab(
          data = data,
          sim_type = "param",
          p = p,
          algorithm = algorithm,
          control_augment = control_augment,
          random_assign_prop = random_assign_prop,
          prior_periods = prior_periods,
          discount_rate = discount_rate,
          simulate_dates = simulate_dates,
          delayed_feedback = delayed_feedback,
          conditions = conditions,
          blocking = blocking,
          clustering = clustering,
          col_names = col_names,
          verbose = verbose,
          ndraws = ndraws,
          starts = period_idxs[["start_idxs"]],
          ends = period_idxs[["end_idxs"]],
          keep_data = keep_data,
          r = r,
          time_model = time_model,
          time_model_args = other_args[["time_model_args"]]
        )
      },
      .options = furrr_opt,
      .progress = verbose
    )
    verbose_log(verbose, "Collating Results")
    results <- condense_results(
      dt = dt || (r * t > 100000),
      keep_data = keep_data,
      mabs = mabs
    )
  }

  results$args <- args
  results$cl <- cl
  return(construct_mab(results, type = "param", multi = r > 1))
}

#' Prepares Data for Simulated MAB
#' @name prep_sim_data
#' @description
#' Initializes the data a simulated MAB trial. Generates block and
#' cluster assignments, allocates all required columns, and assigns treatments and
#' outcomes for the first period using equal assignment probabilities.
#' @inheritParams simulate_mab
#' @inheritParams run_mab
#' @param period_idxs List containing vectors which map their entries to the starting row and ending
#' row of each period.
#' @param equal_probs Vector of equal assignment probabilities.
#' @param simulate_dates Logical; whether or not new success dates should be generated using
#' `time_model`. Does not guarantee these new dates are used for assignment, `delayed_feedback` controls
#' that behavior.
#'
#' @returns Initalized `data.table` or `tibble` with the first period simulation conducted, and all
#' required columns for [run_mab()]

prep_sim_data <- function(
  n,
  p,
  blocks = NULL,
  clusters = NULL,
  blocking,
  clustering,
  conditions,
  equal_probs,
  period_idxs,
  simulate_dates,
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
    block = if (!is.null(blocks_clusters[["blocks"]])) {
      as.character(blocks_clusters[["blocks"]])
    } else {
      NULL
    },
    cluster = if (!is.null(blocks_clusters[["clusters"]])) {
      as.character(blocks_clusters[["clusters"]])
    } else {
      NULL
    },
    assignment_date = assignment_dates,
    mab_condition = rep(NA_character_, n),
    mab_success = rep(NA_real_, n),
    assignment_type = rep(NA_character_, n)
  )

  if (simulate_dates) {
    cols[["new_success_date"]] <- rep(as.Date(NA), n)
  }

  data <- do.call(df_func, cols)

  data <- assign_treatments(
    current_data = data[current_idx, ],
    probs = equal_probs,
    blocking = blocking,
    clustering = clustering,
    conditions = conditions,
    random_assign_prop = 0,
    sim_type = "param",
    cluster_col = "cluster"
  ) |>
    generate_outcomes(
      p = p,
      idx = current_idx,
      data = data,
      simulate_dates = simulate_dates,
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
    period_sizes = period_sizes,
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
#' Uses [methods::formalArgs()] to match arguments provided to `...` of [simulate_mab()] to [furrr::furrr_options()] and the user specified `time_model`
#' @returns A named list with 2 elements, `furr_args` and `time_model_args` each a list of the respective arguments to
#' [furrr::furrr_options()] and the user specified `time_model`
#' @keywords internal

split_args <- function(time_model = NULL, ...) {
  all_args <- rlang::dots_list(..., .named = TRUE)
  furrr_args <- all_args[
    names(all_args) %in% methods::formalArgs(furrr::furrr_options)
  ]
  time_model_args <- if (!is.null(time_model)) {
    all_args[names(all_args) %in% methods::formalArgs(time_model)]
  } else {
    NULL
  }
  return(list(
    furrr_args = furrr_args,
    time_model_args = time_model_args
  ))
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
    extract_mat <- matrix(
      data = c(conditions, other_idx),
      ncol = 2
    )
    p[extract_mat]
  } else {
    return(p[conditions, ])
  }
}

#' Generate Outcomes Per-Unit
#' @name generate_outcomes
#' @description
#' Uses provided success probabilities to draw a Bernoulli outcome for each unit. If `time_model` is provided, it is also used to compute
#' dates of success
#' @inheritParams impute_outcomes
#' @inheritParams simulate_mab
#' @inheritParams prep_sim_data
#' @inheritParams run_mab
#' @inheritParams compute_prior
#' @returns Updated `data` object containing all the outcomes generated in the period, such as the treatment assignments, treatment outcomes. and new success dates

generate_outcomes <- function(
  current_data,
  data,
  p,
  idx,
  simulate_dates,
  time_model = NULL,
  time_model_args = NULL
) {
  conditions <- current_data[["mab_condition"]]
  probs <- extract_success_prob(
    p = p,
    conditions = conditions,
    other_idx = current_data[["cluster"]] %||% current_data[["block"]]
  )

  outcomes <- stats::rbinom(
    nrow(current_data),
    1,
    prob = probs
  )
  success_times <- if (simulate_dates) {
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
  is_dt <- data.table::is.data.table(current_data)

  if (is_dt) {
    current_data[, mab_success := outcomes]
  } else {
    current_data[["mab_success"]] <- outcomes
  }

  if (simulate_dates) {
    if (is_dt) {
      current_data[, new_success_date := assignment_date + success_times]
    } else {
      current_data[["new_success_date"]] <- current_data[["assignment_date"]] +
        success_times
    }
    modified_cols <- c(modified_cols, "new_success_date")
  }

  if (is_dt) {
    data[idx, (modified_cols) := current_data[, modified_cols, with = FALSE]]
  } else {
    data[idx, ] <- current_data
  }
  invisible(data)
}

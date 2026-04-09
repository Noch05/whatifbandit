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
#' @param time_model An optional function with signature `function(n, conditions, successes,
#' current_period, blocks = NULL, clusters = NULL, ...)`
#' that returns a vector of [lubridate::period] objects which will then be added to `dates_of_assignment` to produce `success_date`. Used to simulate delayed feedback mechanism
#' during the trial, so outcomes are imperfectly observed. Only used when`dates_of_assignment` is also supplied. Dates can be generated even when `delayed_feedback == FALSE`,
#' but they will not be used. Default `NULL`. Other optional arguments Cannot share names with arguments in [furrr::furrr_options()].
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
  algorithm <- tolower(algorithm)
  args <- mget(methods::formalArgs(simulate_mab))
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

  setup <- setup_mab_sim(
    n = n,
    t = t,
    p = p,
    blocks = blocks,
    clusters = clusters,
    assignment_dates = assignment_dates,
    time_model = time_model,
    period_sizes = period_sizes
  )
  args <- utils::modifyList(
    args,
    list(
      col_names = setup$col_names,
      equal_probs = setup$equal_probs,
      period_idxs = setup$period_idxs,
      conditions = setup$conditions,
      simulate_dates = setup$simulate_dates
    )
  )

  if (!"control" %in% names(setup$conditions) && control_augment > 0) {
    rlang::abort(c(
      "a Control group must be specified when `control_augment` > 0",
      "x" = sprintf(
        "Treatment conditions specified: %s",
        paste(setup$conditions, sep = ", ")
      ),
      "x" = paste0("Control Augment: ", control_augment)
    ))
  }

  furrr_opt <- do.call(
    furrr::furrr_options,
    c(list(seed = TRUE), other_args$furrr_args)
  )
  run_single <- purrr::partial(
    run_mab_single,
    sim_type = "param",
    algorithm = algorithm,
    control_augment = control_augment,
    random_assign_prop = random_assign_prop,
    prior_periods = prior_periods,
    delayed_feedback = delayed_feedback,
    discount_rate = discount_rate,
    conditions = setup[["conditions"]],
    blocking = setup[["blocking"]],
    clustering = setup[["clustering"]],
    col_names = setup[["col_names"]],
    ndraws = ndraws,
    keep_data = keep_data,
    verbose = verbose,
    r = r,
    time_model = time_model,
    time_model_args = other_args[["time_model_args"]],
    p = setup[["p"]],
    n = n,
    dt = dt,
    blocks = blocks,
    clusters = clusters,
    equal_probs = setup[["equal_probs"]],
    assignment_dates = setup[["assignment_dates"]],
    simulate_dates = setup[["simulate_dates"]],
    period_idxs = setup[["period_idxs"]]
  )
  verbose_log(verbose, "Starting Simulations")
  if (r == 1) {
    results <- run_single()
  } else if (r > 1) {
    mabs <- furrr::future_map(
      seq_len(r),
      run_single,
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
  results$furrr <- furrr_opt
  results$args$time_model_args <- other_args$time_model$args
  return(construct_mab(results, type = "param", multi = r > 1))
}

#' Set Up MAB Simulation
#' @name setup_mab_sim
#' @description
#' Perfoms all one-time set-up requried for [simulate_mab()] as opposed to
#' [prep_sim_data()] which needs to be re-run each period.
#' @inheritParams simulate_mab
#' @returns A named list containing:
#' \itemize{
#'   \item `period_idxs`: A list of 2 integer vectors of period boundary indicies.
#'   \item `assignment_dates`: Vector of assignment dates based on provided dates.
#'   \item `blocking`: Logical; `TRUE` if `blocks` is non-null.
#'   \item `clustering`: Logical; `TRUE` if `clusters` is non-null.
#'   \item `simulate_dates`: Logical; `TRUE` if both `time_model` is a function
#'   and `assignment_dates` is non-null.
#'   \item `p`: The success probability matrix with lowercase and sorted rownames
#'   with rows reordered to match `conditions`.
#'   \item `conditions`: A named character vector of arm labels sorted
#'   alphabetically, with names `"control"` or `"treatment"` as appropriate.
#'   \item `equal_probs`: A named numeric vector of equal assignment
#'   probabilities `1 / K` for each of the `K` arms.
#'   \item `col_names`: A fixed named list of output column name strings.
#' }
#' @family param
#' @keywords internal
setup_mab_sim <- function(
  n,
  t,
  p,
  blocks,
  clusters,
  assignment_dates,
  time_model,
  period_sizes
) {
  period_idxs <- generate_period_idx(n = n, t = t, period_sizes = period_sizes)
  assignment_dates <- generate_assignment_dates(
    n = n,
    assignment_dates = assignment_dates
  )

  blocking <- !is.null(blocks)
  clustering <- !is.null(clusters)
  simulate_dates <- is.function(time_model) && !is.null(assignment_dates)

  rownames(p) <- tolower(rownames(p))
  conditions <- sort(rownames(p))
  names(conditions) <- ifelse(conditions == "control", "control", "treatment")
  p <- p[conditions, , drop = FALSE]
  equal_probs <- stats::setNames(rep(1 / nrow(p), nrow(p)), conditions)

  col_names <- list(
    cluster_col = "cluster",
    assignment_date_col = "assignment_date",
    success_date_col = "success_date"
  )

  list(
    period_idxs = period_idxs,
    assignment_dates = assignment_dates,
    blocking = blocking,
    clustering = clustering,
    simulate_dates = simulate_dates,
    p = p,
    conditions = conditions,
    equal_probs = equal_probs,
    col_names = col_names
  )
}


#' Split Function Arguments
#' @name split_args
#' @inheritParams simulate_mab
#' @description
#' Uses [methods::formalArgs()] to match arguments provided to `...` of [simulate_mab()] to [furrr::furrr_options()] and the user specified `time_model`
#' @returns A named list with 2 elements, `furr_args` and `time_model_args` each a list of the respective arguments to
#' [furrr::furrr_options()] and the user specified `time_model`
#' @keywords internal
#' @family param

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

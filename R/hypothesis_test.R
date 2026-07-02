#' @title Joint Hypothesis Test for Multi-Arm Bandit Trials
#' @name joint_test
#' @description Conducts a joint hypothesis test of no treatment effects across all arms, i.e. that all arms
#' have the same true probability of success, either using a bootstrap procedure or the randomization inference
#' procedure adapted from
#' \href{https://onlinelibrary.wiley.com/doi/abs/10.1111/ajps.12597}{Offer-Westort et al. (2021)}.
#' See details for a description of both methods
#' @param mab A `single_rct_mab` or `single_param_mab` object.
#' @param method A character string; either `"bootstrap"` or `"randomization"`.
#' @param r A positive integer; number of simulations used to build the null distribution.
#' Default is 1000.
#'
#' @return A named list object containing
#' \itemize{
#'   \item `f_statistic`: The observed F-statistic from the IPW regression.
#'   \item `null_distribution`: A numeric vector of F-statistics under the null.
#'   \item `p_value`: The proportion of simulated F-statistics more extreme than observed.
#'   \item `method`: The method used.
#'   \item `r`: Number of replications used.
#' }
#' @export
#' @details
#'
#' `method = "randomization"` operates under the a sharp null that each unit
#' would express the same outcome no matter the treatment they were assigned. To achieve this
#' the trial is re-simulated but new outcomes are not generated or imputed, however the adaptive algorithm
#' still changes the assignments. This results in a null distribution that captures how the adaptive
#' algorithm will assign even when the outcomes are not related to treatments at all. This test is not
#' valid for resimulated random trials.
#'
#' `method = "bootstrap"` operates under the null hypothesis that there is no difference between
#' treatment arms within each each block/cluster the
#' true success probability is the same for any treatment. To generate this distribution,
#' the trial is re-simulated using an appropriate `p` matrix which satisfies the null. For each
#' simulated F-statistic, the `p` matrix is redrawn. This is achieved by drawing a single
#' probaiblity for each column of `p` matrix from the posterior beta distribution of the original
#' trial. For an RCT this is the original data, and for the simulated MAB it is the simulated outcomes.
#' This preserves
#' any potential heterogeneity across the block or cluster structure, but still ensures
#' no treatment effect is specified. Drawing a new `p` matrix each time properly captures
#' the uncertainty for the null distribution, using the best estimate available.
#'
#' For `method == "boostrap"` with a `single_rct_mab`, the block and or cluster assignment
#' proportions are taken from the original dataset.
#'
#'
#' @references
#' Offer-Westort, Molly, Alexander Coppock, and Donald P. Green.
#' "Adaptive Experimental Design: Prospects and Applications in Political Science."
#'  American Journal of Political Science 65, no. 4 (2021): 826–44. \doi{10.1111/ajps.12597}.
#' @examples
#'
#' data(tanf)
#' set.seed(5)
#' adaptive <- mab_from_rct(success ~ condition, data = tanf, algorithm = "thompson",
#' period_method = "batch", period_length = 500)
#'
#' # Low `r` for examples, use replications in practice
#' joint_test(adaptive, "randomization", r = 2)
#' joint_test(adaptive, "bootstrap", r = 2)
#'
#'
joint_test <- function(mab, method, r = 1000) {
  check_posint(r)
  if (!inherits(mab, "single_mab")) {
    rlang::abort(c("Joint-tests can only be performed on `single_mab` objects"))
  }
  if (method == "randomization" && inherits(mab, "singe_rct_mab")) {
    rlang::warn(c(
      "Randomization inference may not be informative for resimulated RCT objects."
    ))
  }

  null <- switch(
    method,
    "bootstrap" = joint_boot_null(mab = mab, r = r),
    "randomization" = joint_random_null(mab = mab, r = r),
    check_string(
      tolower(method),
      valid = c("bootstrap", "randomization"),
      "method"
    )
  )
  f <- mab$estimates$mean[
    mab$estimates$mab_condition == "Joint-F" && mab$estimates$estimator == "IPW"
  ]

  p <- mean(null >= f, na.rm = TRUE)

  return(list(
    f_stat = f,
    null_distribution = null,
    p_value = p,
    method = method,
    r = r
  ))
}

#' Helpers for Joint F Test
#' @name f_helpers
#' @description Takes the `single_mab` object provided and returns the proper
#' null distribution for the randomization or the bootstrap joint test.
#' @returns a numeric vector of simulated F-statistics
NULL

#' @describeIn f_helpers Prepares arguments for the randomization joint test.
#' @inheritParams joint_test
#' @keywords internal
joint_random_null <- function(mab, r) {
  args <- joint_base_args(mab, sim_type = "test")

  na_rows <- args$period_idxs$start_idxs[2]:nrow(args$data)
  if (data.table::is.data.table(args$data)) {
    args$data[na_rows, mab_condition := NA_character_]
  } else {
    args$data[["mab_condition"]][na_rows] <- NA_character_
  }

  furrr::future_map_dbl(
    seq_len(r),
    \(.) joint_null_inner(args),
    .options = mab$config$parallel,
    .progress = mab$config$args$verbose
  )
}

#' @describeIn f_helpers Prepares arguments for the parametric bootstrap joint test.
#' @inheritParams joint_test
#' @keywords internal
joint_boot_null <- function(mab, r) {
  args <- joint_base_args(mab, sim_type = "param")

  if (inherits(mab, "single_rct_mab")) {
    col_names <- args$col_names

    time_model_args <- build_time_model_args(mab, args, col_names)

    args <- utils::modifyList(
      args,
      list(
        assignment_dates = if (is.null(col_names$assignment_date_col)) {
          NULL
        } else {
          mab$new_data[[col_names$assignment_date_col]]
        },
        blocks = if (is.null(mab$config$args$blocks)) {
          NULL
        } else {
          group_prop(mab$new_data, "block")
        },
        clusters = build_rct_clusters(mab, col_names),
        n = nrow(mab$new_data),
        dt = data.table::is.data.table(mab$new_data),
        equal_probs = rep(1 / length(args$conditions), length(args$conditions)),
        simulate_dates = mab$config$args$delayed_feedback,
        col_names = list(
          cluster_col = "cluster",
          assignment_date_col = "assignment_date",
          success_date_col = "success_date"
        ),
        time_model = time_model_args$time_model,
        time_model_args = time_model_args$args,
        whole_experiment = NULL,
        data = NULL
      )
    )

    success_col <- col_names$success_col
    group_col <- if (!is.null(args$clusters)) {
      col_names$cluster_col
    } else if (!is.null(args$blocks)) {
      "block"
    } else {
      NULL
    }
    dn <- list(mab$config$args$conditions, sort(names(build_p_cols(args))))
    print(dn)
  } else {
    success_col <- "mab_success"
    group_col <- if (!is.null(args$clusters)) {
      "cluster"
    } else if (!is.null(args$blocks)) {
      "block"
    } else {
      NULL
    }
    dn <- dimnames(mab$config$args$p) |> lapply(sort)
  }

  build_p <- boot_build_p(
    data = mab$new_data,
    success_col = success_col,
    group_col = group_col,
    cols = build_p_cols(args)
  )

  null <- furrr::future_map_dbl(
    seq_len(r),
    \(.) {
      args[["p"]] <- stats::setNames(
        stats::rbeta(
          length(build_p$cols),
          shape1 = build_p$s + 1,
          shape2 = (build_p$n - build_p$s) + 1
        ),
        names(dn[[2]])
      ) |>
        rep(length(dn[[1]])) |>
        matrix(
          nrow = length(build_p$cols),
          dimnames = list(dn[[2]], dn[[1]])
        ) |>
        t()
      joint_null_inner(args = args)
    },
    .options = mab$config$parallel,
    .progress = mab$config$args$verbose
  )
}

#' Build Proper Arguments for RCT Boostrap Joint Test
#' @name build_rct
#' @keywords internal
#'
NULL

#' @describeIn build_rct Resolves the `clusters` argument for a `single_rct_mab` bootstrap.
#' When both blocks and clusters are present, clusters is a named list of
#' per-block cluster proportion vectors (as documented in [simulate_mab()]).
#' When only clusters are present, returns a flat named proportion vector via
#' [group_prop()]. Returns `NULL` when no clustering was used.
#' @param mab A `single_rct_mab` object.
#' @param col_names Named list of column name strings from `args$col_names`.
#' @returns A named numeric vector, named list of vectors, or `NULL`.
#' @keywords internal
build_rct_clusters <- function(mab, col_names) {
  if (is.null(mab$config$args$clusters)) {
    return(NULL)
  }
  if (!is.null(mab$config$args$blocks)) {
    data <- mab$new_data
    blocks <- unique(
      if (data.table::is.data.table(data)) data[["block"]] else data$block
    )
    lapply(
      stats::setNames(blocks, blocks),
      \(b) {
        block_data <- if (data.table::is.data.table(data)) {
          data[block == b]
        } else {
          data[data$block == b, ]
        }
        group_prop(block_data, col_names$cluster_col)
      }
    )
  } else {
    group_prop(mab$new_data, col_names$cluster_col)
  }
}

#' @describeIn build_rct Returns the named vector of proportions that drives the columns of the null
#' `p` matrix — clusters if present, blocks if present, or a scalar `1` for
#' the no-blocking/no-clustering case. For the blocked-and-clustered case,
#' clusters is a named list; this flattens it to a single named vector (since
#' the p matrix columns are individual clusters, not blocks).
#' @param args The processed args list from [joint_base_args()] (or after
#'   [utils::modifyList()]) which contains `$clusters` and `$blocks`.
#' @returns A named numeric vector of proportions, or a scalar `1`.
#' @keywords internal
build_p_cols <- function(args) {
  if (!is.null(args$clusters)) {
    if (is.list(args$clusters)) {
      unlist(unname(args$clusters))
    } else {
      args$clusters
    }
  } else if (!is.null(args$blocks)) {
    args$blocks
  } else {
    c(`1` = 1)
  }
}


#' @describeIn build_rct Recovers successes and totals for each column group (cluster, block, or
#' the whole dataset), then returns them alongside the resolved column
#' proportions vector for use in [joint_boot_null()].
#' @param data Input data.
#' @param success_col Name of the success column.
#' @param group_col Column to group by, or `NULL` for the whole dataset.
#' @param cols Named numeric vector of column proportions (from [build_p_cols()]).
#' @returns A list with elements `cols`, `s` (successes), and `n` (totals),
#'   all named and sorted consistently.
#' @keywords internal
boot_build_p <- function(data, success_col, group_col, cols) {
  counts <- boot_null_counts(data, success_col, group_col)
  if (!is.null(group_col)) {
    s <- as_named_vec(counts, val = "s", name = group_col)
    n <- as_named_vec(counts, val = "n", name = group_col)
    list(cols = cols, s = s[order(names(s))], n = n[order(names(n))])
  } else {
    list(cols = cols, s = counts$s, n = counts$n)
  }
}

#' @describeIn build_rct Constructs the `time_model` function and its associated argument list when
#' `delayed_feedback` is enabled for a `single_rct_mab` bootstrap. Returns a
#' list with `time_model = NULL` and `args = list()` when delayed feedback is
#' not in use.
#' @param mab A `single_rct_mab` object.
#' @param args The processed args list from [joint_base_args()].
#' @param col_names Named list of column name strings.
#' @returns A list with elements `time_model` (function or `NULL`) and `args`
#'   (list of additional arguments for the time model).
#' @keywords internal
build_time_model_args <- function(mab, args, col_names) {
  if (!args$delayed_feedback) {
    return(list(
      time_model = NULL,
      args = list(impute_dates = NULL, original_dates = NULL)
    ))
  }

  impute_dates <- precompute_imputation(
    data = mab$new_data,
    whole_experiment = TRUE,
    delayed_feedback = args$delayed_feedback,
    col_names = col_names
  )[["dates"]]

  original_dates <- if (data.table::is.data.table(mab$new_data)) {
    mab$new_data[, .("period_number", col_names$assignment_date_col)] |>
      split(by = "period_number") |>
      lapply(\(x) x[[col_names$assignment_date_col]])
  } else {
    mab$new_data |>
      dplyr::select(dplyr::all_of(c(
        "period_number",
        col_names$assignment_date_col
      ))) |>
      dplyr::group_split(period_number) |>
      lapply(\(x) x[[col_names$assignment_date_col]])
  }

  time_model <- function(
    n,
    conditions,
    successes,
    current_period,
    blocks = NULL,
    clusters = NULL,
    impute_dates,
    original_dates
  ) {
    treatment_block <- paste(conditions, successes, sep = "_")
    dates <- impute_dates[[current_period]][treatment_block]
    org <- original_dates[[current_period]]
    return(dates - org)
  }

  list(
    time_model = time_model,
    args = list(impute_dates = impute_dates, original_dates = original_dates)
  )
}

#' @describeIn f_helpers Extracts common arguments from [run_mab_single()] and the
#' `single_mab` provided object's arguments slot.
#' @returns A named list of arguments to be used for [run_mab_single()]
#' @inheritParams joint_test
#' @inheritParams run_mab
#' @keywords internal
joint_base_args <- function(mab, sim_type) {
  args <- mab$config$args[intersect(
    names(mab$config$args),
    methods::formalArgs(run_mab_single)
  )] |>
    utils::modifyList(
      list(
        sim_type = sim_type,
        blocking = !is.null(mab$config$args$blocks),
        clustering = !is.null(mab$config$args$clusters),
        estimators = "ipw",
        contrasts = NULL
      )
    )
  if (sim_type == "param") {
    args$p <- NULL
  }
  if (sim_type == "test") {
    args$data <- mab$new_data
  }
  return(args)
}

#' @describeIn f_helpers inner function for [furrr::future_map()]
#' @param args Arguments list to [run_mab_single()]
#' @returns The F-statistic from the IPW regression of the MAB Trial
#' @keywords internal
joint_null_inner <- function(args) {
  estimates <- do.call(run_mab_single, args)[["estimates"]]
  f <- estimates[["mean"]][estimates[["mab_condition"]] == "Joint-F"]
  return(f)
}

#' Get Group Proportions
#' @description Accepts input data, and a group column, and returns the proportion of the data that belongs
#' to each group
#'
#' @param data Input Data
#' @param group Column to group by
#'
#' @returns A named numeric vector with the `names` corresponding to the group, and the value
#' to its proportion among the provided data.
#' @keywords internal
#'
group_prop <- function(data, group) {
  UseMethod("group_prop", data)
}

#' @rdname group_prop
#' @method group_prop data.frame
#' @export
group_prop.data.frame <- function(data, group) {
  n <- nrow(data)
  data |>
    dplyr::group_by(!!rlang::sym(group)) |>
    dplyr::summarize(size = dplyr::n() / n) |>
    as_named_vec(val = "size", name = group)
}

#' @rdname group_prop
#' @method group_prop data.table
#' @export
group_prop.data.table <- function(data, group) {
  n <- nrow(data)
  data[, .(size = .N / n), by = group] |>
    as_named_vec(val = "size", name = group)
}


#' Recover Block-Specific Success and Total Counts for Bootstrap Null
#' @name boot_null_counts
#'
#' @description
#' Recovers the number of successes and total observations within each group
#' for use in constructing group-specific Beta posteriors for the parametric
#' bootstrap joint test.
#'
#' @param data Data holding the appropriate outcomes
#' @param success_col Column holding the outcomes.
#' @param group Column to group by, or `NULL` for the whole dataset.
#'
#' @returns An aggregated data.frame or data.table with the appropriate counts.
#'
#' @keywords internal
boot_null_counts <- function(data, success_col, group = NULL) {
  UseMethod("boot_null_counts", data)
}

#' @rdname boot_null_counts
#' @method boot_null_counts data.frame
#' @export
boot_null_counts.data.frame <- function(data, success_col, group = NULL) {
  if (!is.null(group)) {
    data |>
      dplyr::group_by(!!rlang::sym(group)) |>
      dplyr::summarize(n = dplyr::n(), s = sum(!!rlang::sym(success_col)))
  } else {
    data |>
      dplyr::summarize(n = dplyr::n(), s = sum(!!rlang::sym(success_col)))
  }
}

#' @rdname boot_null_counts
#' @method boot_null_counts data.table
#' @export
boot_null_counts.data.table <- function(data, success_col, group = NULL) {
  if (!is.null(group)) {
    data[, .(n = .N, s = sum(get(success_col))), by = group]
  } else {
    data[, .(n = .N, s = sum(get(success_col)))]
  }
}
#----------------------------------------------------------------------------#
#' @title Pairwise and Univariate Hypothesis Tests for Multi-Armed Bandits
#' @name pairwise_test
#'
#' @description
#' Performs two-way univariate or pairwise hypothesis tests for treatment-arm means or treatment
#' effects. When a `multi_mab` object is supplied, hypothesis test results are returned
#' for every trial.
#'
#' @param mab An object inheriting from class `.mab`.
#' @param arm1 A string specifying the treatment arm to test.
#' @param arm2 An optional string specifying the comparison treatment arm. If omitted,
#'   a one-sample test is performed on `arm1`.
#' @param H0 The null hypothesis value. For univariate tests this is the hypothesized
#'   mean; for pairwise tests it is the hypothesized difference in means.
#' @param conf Confidence level, default is 95%.
#' @param direction String specifying test direction, only "twoway" is available, one-way tests are planned
#' for future updates.
#' @param estimator A character vector specifying the estimator to use. Supported
#'   values are currently only `"AIPW"`, with the others planned for future updates.
#'
#' @details
#' Hypothesis tests based on the AIPW estimator use the standard normal distribution,
#' following href{https://www.pnas.org/doi/full/10.1073/pnas.2014602118}{Hadad et al. (2021)}.
#' Two-sample tests are always conducted as arm1 - arm2.
#'
#' @return A named list object containing
#' \itemize{
#'   \item `tests`: A data.frame of the test results with columns containing the estimates, test
#'   statistics, p-values, and confidence intervals.
#'   \item `null_value`: Null value for the test.
#'   \item `direction`: Direction of the test.
#'   \item `method`: The method used.
#'   \item `estimator`: Estimator used.
#' }
#'
#' @references
#' Hadad, Vitor, David A. Hirshberg, Ruohan Zhan, Stefan Wager, and Susan Athey. 2021.
#' "Confidence Intervals for Policy Evaluation in Adaptive Experiments."
#' \emph{Proceedings of the National Academy of Sciences of the United States of America}
#' 118 (15): e2014602118. \doi{10.1073/pnas.2014602118}.
#'
#' Offer-Westort, Molly, Alexander Coppock, and Donald P. Green. 2021.
#' "Adaptive Experimental Design: Prospects and Applications in Political Science."
#' \emph{American Journal of Political Science} 65 (4): 826--844.
#' \doi{10.1111/ajps.12597}.
#'
#' @export
#'
pairwise_test <- function(
  mab,
  arm1,
  arm2 = NULL,
  H0 = 0,
  conf = 0.95,
  direction = "twoway",
  estimator = "AIPW"
) {
  check_string(tolower(estimator), valid = "aipw", "estimator")
  check_string(tolower(direction), valid = "twoway", "direction")
  check_string(arm1, valid = unique(mab$estimates$mab_condition), "arm1")
  if (!is.null(arm2)) {
    check_string(arm2, valid = unique(mab$estimates$mab_condition), "arm2")
  }
  if (!is.numeric(H0)) {
    rlang::abort(
      c("H0 must be a number", "x" = paste0("You Provided: ", H0))
    )
  }
  check_prop(conf)

  estimates <- if (data.table::is.data.table(mab$estimates)) {
    mab$estimates[mab_condition %in% c(arm1, arm2) & estimator %in% estimator]
  } else {
    mab$estimates[
      mab$estimates$mab_condition %in%
        c(arm1, arm2) &
        mab$estimates$estimator %in% estimator,
    ]
  }

  est <- if (is.null(arm2)) {
    one_sample_test(estimates)
  } else {
    two_sample_test(estimates, arm1, arm2)
  }
  alpha <- (1 - conf)
  q <- qnorm(alpha / 2, lower.tail = FALSE)

  test_stat <- (est$est - H0) / est$se
  p <- pnorm(abs(test_stat), lower.tail = FALSE)

  return(list(
    tests = tibble::tibble(
      estimate = est$est,
      statistic = test_stat,
      p_value = p,
      low = est$est - q * est$se,
      high = est$est + q * est$se
    ),
    null_value = H0,
    direction = direction,
    method = if (is.null(arm2)) "One Sample" else "Two Sample",
    estimator = estimator
  ))
}

#' Internal hypothesis test helpers
#'
#' Helper functions used by [pairwise_test()] to compute one- and two-sample
#' estimates and their standard errors for different estimate storage classes.
#'
#' These functions are intended for internal use only.
#'
#' @name hypothesis_test_helpers
#' @keywords internal
NULL

#' @rdname hypothesis_test_helpers
#' @keywords internal
one_sample_test <- function(est) {
  list(
    est = est[["mean"]],
    se = est[["se"]]
  )
}
#' @rdname hypothesis_test_helpers
#' @keywords internal
two_sample_test <- function(est, arm1, arm2, H0) {
  UseMethod("two_sample_test", est)
}
#' @rdname hypothesis_test_helpers
#' @keywords internal
#' @export
two_sample_test.data.frame <- function(est, arm1, arm2, H0) {
  list(
    est = (est$mean[est$mab_condition == arm1] -
      est$mean[est$mab_condition == arm2]),
    se = sqrt(
      est$se[est$mab_condition == arm1]^2 +
        est$se[est$mab_condition == arm2]^2
    )
  )
}
#' @rdname hypothesis_test_helpers
#' @keywords internal
#' @export
two_sample_test.data.table <- function(est, arm1, arm2, H0) {
  list(
    est = est[mab_condition == arm1, mean] -
      est[mab_condition == arm2, mean],
    se = sqrt(
      est[mab_condition == arm1, se]^2 +
        est[mab_condition == arm2, se]^2
    )
  )
}

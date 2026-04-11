#------------------------------------------------------------
#' Constructs `mab` and its other class variants
#' @name construct_mab
#' @description Simple construction for proper `mab` subclasses as output
#' to [simulate_mab()] and [mab_from_rct()].
#' @param mab Named list output of [simulate_mab()] or [mab_from_rct()].
#' @param type Type of simulated trial, either `"rct"` or `"param"` to denote whether it was an rct re-simulation or an simulation form population parameters.
#' @param multi Logical; `TRUE` denotes multiple trials.
#' @returns Input `mab` with appropriate S3 class, restructured for output
#' @keywords internal

construct_mab <- function(mab, type, multi) {
  class <- if (multi) {
    c(paste0("multi_", type, "_mab"), "multi_mab")
  } else {
    c(paste0("single_", type, "_mab"), "single_mab")
  }
  structure(
    list(
      new_data = mab$final_data,
      bandit = list(
        statistic = mab$bandits,
        assignment_prob = mab$assignment_prob,
        assignment_quant = mab$assignment_quantities
      ),
      estimates = mab$estimates,
      config = list(args = mab$args, call = mab$cl, parallel = mab$furrr)
    ),
    class = c(class, ".mab", "list")
  )
}


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
  f <- mab$estimates$mean[mab$estimates$mab_condition == "Joint-F"]

  p <- mean(null >= f, na.rm = TRUE)

  return(list(
    f_stat = f,
    null_distribution = null,
    p_value = p,
    method = method
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

  if (data.table::is.data.table(args$data)) {
    args$data[
      args$period_idxs$start_idxs[2]:nrow(args$data),
      mab_condition := NA_character_
    ]
  } else {
    args$data[["mab_condition"]][
      args$period_idxs$start_idxs[2]:nrow(args$data)
    ] <- NA_character_
  }
  null <- furrr::future_map_dbl(
    seq_len(r),
    \(.) joint_null_inner(args),
    .options = mab$config$parallel,
    .progress = mab$config$args$verbose
  )

  return(null)
}

#' @describeIn f_helpers Prepares arguments for the parametric bootstrap joint test.
#' @inheritParams joint_test
#' @keywords internal
joint_boot_null <- function(mab, r) {
  args <- joint_base_args(mab, sim_type = "param")

  if (inherits(mab, "single_rct_mab")) {
    col_names <- args$col_names
    if (args$delayed_feedback) {
      impute_dates <- precompute_imputation(
        data = mab$new_data,
        whole_experiment = TRUE,
        delayed_feedback = args$delayed_feedback,
        col_names = col_names
      )[["dates"]]
      original_dates <- if (data.table::is.data.table(mab$new_data)) {
        mab$new_data[, .("period_number", col_names$assignment_date_col)] |>
          split(by = "period_number") |>
          lapply(\(x) {
            x[[col_names$assignment_date_col]]
          })
      } else {
        mab$new_data |>
          dplyr::select(dplyr::all_of(c(
            "period_number",
            col_names$assignment_date_col
          ))) |>
          dplyr::group_split(period_number) |>
          lapply(\(x) {
            x[[col_names$assignment_date_col]]
          })
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
    } else {
      time_model <- NULL
      impute_dates <- NULL
      original_dates <- NULL
    }
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
        clusters = if (is.null(mab$config$args$clusters)) {
          NULL
        } else {
          group_prop(mab$new_data, col_names$cluster_col)
        },
        n = nrow(mab$new_data),
        dt = data.table::is.data.table(mab$new_data),
        equal_probs = rep(1 / length(args$conditions), length(args$conditions)),
        simulate_dates = mab$config$args$delayed_feedback,
        col_names = list(
          cluster_col = "cluster",
          assignment_date_col = "assignment_date",
          success_date_col = "success_date"
        ),
        time_model = time_model %||% NULL,
        time_model_args = list(
          impute_dates = impute_dates %||% NULL,
          original_dates = original_dates %||% NULL
        ),
        whole_experiment = NULL,
        data = NULL
      )
    )
    rows <- length(mab$config$args$conditions)
    get_counts <- purrr::partial(
      boot_null_counts,
      data = mab$new_data,
      success_col = col_names$success_col
    )

    build_p <- if (!is.null(args$clusters)) {
      counts <- get_counts(col_names$cluster_col)
      s <- as_named_vec(counts, val = "s", name = col_names$cluster_col)
      n <- as_named_vec(counts, val = "n", name = col_names$cluster_col)
      list(cols = args$clusters, s = s[order(names(s))], n = n[order(names(n))])
    } else if (!is.null(args$blocks)) {
      counts <- get_counts("block")
      s <- as_named_vec(counts, val = "s", name = "block")
      n <- as_named_vec(counts, val = "n", name = "block")
      list(cols = args$blocks, s = s[order(names(s))], n = n[order(names(n))])
    } else {
      counts <- get_counts()
      list(cols = 1, s = counts$s, n = counts$n)
    }
    cols <- length(build_p$cols)
    dn <- list(mab$config$args$conditions, sort(names(build_p$cols)))
  } else {
    cols <- ncol(mab$config$args$p)
    rows <- nrow(mab$config$args$p)
    dn <- dimnames(mab$config$args$p) |> lapply(\(x) sort(x) |> tolower())
    get_counts <- purrr::partial(
      boot_null_counts,
      data = mab$new_data,
      success_col = "mab_success"
    )
    build_p <- if (!is.null(args$clusters)) {
      counts <- get_counts("cluster")
      s <- as_named_vec(counts, val = "s", name = "cluster")
      n <- as_named_vec(counts, val = "n", name = "cluster")
      list(s = s[order(names(s))], n = n[order(names(n))])
    } else if (!is.null(args$blocks)) {
      counts <- get_counts("block")
      s <- as_named_vec(counts, val = "s", name = "block")
      n <- as_named_vec(counts, val = "n", name = "block")
      list(s = s[order(names(s))], n = n[order(names(n))])
    } else {
      counts <- get_counts()
      list(s = counts$s, n = counts$n)
    }
  }

  null <- furrr::future_map_dbl(
    seq_len(r),
    \(.) {
      args[["p"]] <- stats::setNames(
        stats::rbeta(
          cols,
          shape1 = build_p$s + 1,
          shape2 = (build_p$n - build_p$s) + 1
        ),
        names(dn[[2]])
      ) |>
        rep(rows) |>
        matrix(nrow = cols, dimnames = list(dn[[2]], dn[[1]])) |>
        t()
      joint_null_inner(args = args)
    },
    .options = mab$config$parallel,
    .progress = mab$config$args$verbose
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
        estimators = "ipw"
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
#'
group_prop.data.frame <- function(data, group) {
  n <- nrow(data)
  data |>
    dplyr::group_by(!!rlang::sym(group)) |>
    dplyr::summarize(size = dplyr::n() / n) |>
    as_named_vec(val = "size", name = group)
}

#' @rdname group_prop
#' @method group_prop data.table
group_prop.data.table <- function(data, group) {
  n <- nrow(data)
  data[, .(size = .N / n), by = group] |>
    as_named_vec(val = "size", name = group)
}


#' Recover Block-Specific Success and Total Counts for Bootstrap Null
#' @name boot_null_counts
#'
#' @description
#' Recovers the number of successes and total observations within each block
#' for use in constructing block-specific Beta posteriors for the parametric
#' bootstrap joint test.
#'
#' @param data Data holding the appropriate outcomes
#' @param ... Columns to group by
#' @param success_col Column holding the outcomes.
#'
#' @returns An aggregated data.frame or data.table, with the appropraite counts.
#'
#' @keywords internal
boot_null_counts <- function(data, success_col, ...) {
  UseMethod("boot_null_counts", data)
}

#' @rdname boot_null_counts
#' @method boot_null_counts data.frame
boot_null_counts.data.frame <- function(data, success_col, ...) {
  cols <- c(rlang::dots_list(...) |> unlist())
  if (!is.null(cols)) {
    data |>
      dplyr::group_by(!!!rlang::syms(cols)) |>
      dplyr::summarize(n = dplyr::n(), s = sum(!!rlang::sym(success_col)))
  } else {
    data |>
      dplyr::summarize(n = dplyr::n(), s = sum(!!rlang::sym(success_col)))
  }
}

#' @rdname boot_null_counts
#' @method boot_null_counts data.table
boot_null_counts.data.table <- function(data, success_col, ...) {
  cols <- c(rlang::dots_list(...) |> unlist())
  if (!is.null(cols)) {
    data[, .(n = .N, s = sum(get(success_col))), by = cols]
  } else {
    data[, .(n = .N, s = sum(get(success_col)))]
  }
}

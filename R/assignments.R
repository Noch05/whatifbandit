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
#' @family assign
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
  sim_type
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
      sim_type = sim_type
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
      sim_type = sim_type
    )
  }
}
#' @describeIn assign_treatments Selects the appropriate `{randomizr}` function and constructs its argument list
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

#' @describeIn assign_treatments Pre-allocates a character vector and fills treatment assignments
#' for bandit and randomly assigned subsets separately, using the appropriate
#' randomizr function built by [build_ra_args()].
#' @inheritParams compute_prior
#' @inheritParams assign_treatments
#' @param band_idx Integer vector of bandit-assigned row indices
#' @param rand_idx Integer vector of randomly-assigned row indices
#' @returns Character vector of length `nrow(current_data)` with treatment assignments.
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
#' @rdname assign_treatments
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
  sim_type
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

  if (sim_type == "resim") {
    current_data[["impute_req"]] <- as.integer(
      as.character(current_data[["mab_condition"]]) !=
        as.character(current_data[[condition_col]])
    )
  }

  return(current_data)
}

#' @method assign_treatments data.table
#' @rdname assign_treatments
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
  sim_type
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

  if (sim_type == "resim") {
    current_data[,
      impute_req := as.integer(
        as.character(mab_condition) != as.character(get(condition_col))
      )
    ]
  }
  return(invisible(current_data))
}

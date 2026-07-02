#' Computing Linear Contrasts
#' @description A set of helper functions for computing linear contrasts of AIPW, IPW, and OLS estimates
#' @name lin_contrast
#' @family estimation
#' @keywords internal
NULL

#' Build Contrast Matrices for Pairwise Comparisons
#' @describeIn lin_contrast Constructs a matrix of contrast vectors contrast matrices for testing pairwise comparisons
#' between treatment arms.
#' @inheritParams run_mab_single
#' @inheritParams mab_from_rct
#' @param bandits data.frame or data.table of bandit statistics
#'
#' @return A matrix of 1 row contrast vectors, encoding a single pairwise comparison.
#' @keywords internal
build_contrast_matrices <- function(
  conditions,
  contrasts,
  bandits
) {
  control_contrasts <- NULL
  best_contrasts <- NULL
  all_contrasts <- NULL

  if (contrasts %in% c("control", "both")) {
    control_idx <- which(names(conditions) == "control")
    control_contrasts <- make_contrasts(
      conditions,
      control_idx,
      type = "control"
    )
  }
  if (contrasts %in% c("best", "both")) {
    best_idx <- if (data.table::is.data.table(bandits)) {
      which.max(bandits[
        nrow(bandits),
        .SD,
        .SDcols = conditions
      ])
    } else {
      which.max(bandits[
        nrow(bandits),
        conditions
      ])
    }
    best_contrasts <- make_contrasts(conditions, best_idx, type = "best")
  }

  if (contrasts == "all") {
    all_contrasts <- clubSandwich::constrain_pairwise(
      seq_len(conditions),
      coefs = conditions
    )
  }

  combined <- do.call(
    rbind,
    c(list(control_contrasts, best_contrasts), all_contrasts)
  )
  return(unique(combined))
}

#' Make Contrast Vectors
#' @describeIn lin_contrast Creates contrast vectors for linear hypothesis test
#' @param conditions Vector of treatment conditions
#' @param ref_idx Index of the reference arm
#' @param type Type of contrats, "best" or "control"
#' @returns A matrix row vectors containing each contrast to test.
#' @keywords internal
make_contrasts <- function(conditions, ref_idx, type) {
  k <- length(conditions)
  others <- setdiff(seq_len(k), ref_idx)

  vapply(
    others,
    \(i) {
      C <- rep(0, k)
      if (type == "best") {
        C[[ref_idx]] <- 1
        C[[i]] <- -1
      } else {
        C[[ref_idx]] <- -1
        C[[i]] <- 1
      }
      return(C)
    },
    numeric(k)
  ) |>
    t()
}

compute_contrast <- function(...) {
  return(0)
}

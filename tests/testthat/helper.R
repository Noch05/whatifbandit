# Helper Function to Generate Data Sets

generate_data <- function(n, k) {
  data <- tibble::tibble(
    type_of_policy = sample(
      c(
        "Control Group",
        "Treatment 1",
        "Treatment 2",
        "Treatment 3",
        "Treatment 4"
      ),
      size = n,
      replace = TRUE
    ),
    city = sample(c("New York", "DC", "Los Angeles"), size = n, replace = TRUE),
    male = rbinom(n, 1, 0.5),
    date_of_treatment = sample(
      seq(
        lubridate::ymd("2024-01-01"),
        lubridate::ymd("2024-06-01"),
        1
      ),
      size = n,
      replace = TRUE
    ),
    successful_return = dplyr::case_when(
      type_of_policy == "Control Group" ~ rbinom(n, 1, 0.3),
      type_of_policy == "Treatment 1" ~ rbinom(n, 1, 0.25),
      type_of_policy == "Treatment 2" ~ rbinom(n, 1, 0.4),
      type_of_policy == "Treatment 3" ~ rbinom(n, 1, 0.35),
      type_of_policy == "Treatment 4" ~ rbinom(n, 1, 0.42),
    ),
    date_returned = dplyr::if_else(
      successful_return == 1,
      date_of_treatment +
        lubridate::days(
          sample.int(n / 2, size = n, replace = TRUE)
        ),
      NA
    ),
    treatment_wave_assignment = lubridate::ymd(
      paste0("2024-0", lubridate::month(date_of_treatment), "-01")
    ),
    months = lubridate::month(date_of_treatment)
  ) |>
    dplyr::arrange(date_of_treatment) |>
    dplyr::mutate(identification_card_num = seq(1, n, 1), )

  static_args <- list(
    data = data,
    data_cols = c(
      id_col = "identification_card_num",
      condition_col = "type_of_policy",
      success_col = "successful_return",
      date_col = "date_of_treatment",
      assignment_date_col = "treatment_wave_assignment",
      success_date_col = "date_returned",
      month_col = "months"
    ),
    control_condition = "Control Group",
    block_cols = c("city", "male"),
    verbose = FALSE,
    ndraws = 500
  )

  full_args <- expand.grid(
    algorithm = c("Thompson", "UCB1"),
    assignment_method = c("Date", "Batch", "Individual"),
    time_unit = c("Day", "Week", "Month"),
    whole_experiment = c(TRUE, FALSE),
    perfect_assignment = c(TRUE, FALSE),
    period_length = c(n / n, n / 10, n / 5),
    prior_periods = c(n / n, n / 10, n),
    control_augment = c(0, 0.25, 0.75),
    random_assign_prop = c(0, 0.25, 0.75),
    blocking = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  ) |>
    dplyr::filter(
      !(assignment_method == "Batch" & period_length == 1) &
        !(control_augment > 0 & random_assign_prop > 0) &
        !(time_unit == "Month" & period_length > 1)
    ) |>
    dplyr::mutate(
      time_unit = dplyr::if_else(
        assignment_method == "Date",
        time_unit,
        NA_character_
      ),
      period_length = dplyr::if_else(
        assignment_method == "Individual",
        NA_real_,
        period_length
      )
    ) |>
    dplyr::distinct(.keep_all = TRUE) |>
    dplyr::group_by(algorithm, assignment_method) |>
    dplyr::slice_sample(n = k) |>
    dplyr::ungroup()
  return(list(
    full_args = full_args,
    static_args = static_args
  ))
}

# Checking Functions
control_augment_checks <- function(output) {
  col <- paste0(output$settings$control, "_assign_prob")
  assign_probs <- output$assignment_probs |>
    dplyr::filter(period_number != 1)
  vec <- output$assign_probs[[col]]
  return(all(vec > output$settings$control_augment))
}
random_assign_checks <- function(output) {
  true_prop <- mean(
    output$final_data$assignment_type[
      output$final_data$assignment_type != "initial"
    ] ==
      "random"
  )
  check <- dplyr::near(
    true_prop,
    output$settings$random_assign_prop,
    tol = 0.15
  )
  return(check)
}

single_mab_checks <- function(output) {
  band_col_check <- length(output$settings$conditions) ==
    (ncol(output$bandits) - 1)

  prob_col_check <- length(output$settings$conditions) ==
    (ncol(output$assignment_probs) - 1)

  est_check <- nrow(output$settings$estimates) ==
    (2 * length(output$settings$conditions))

  anyNA_ests <- dplyr::summarize(
    output$final_data,
    dplyr::across(
      dplyr::starts_with("aipw"),
      ~ sum(is.na(.x))
    )
  ) |>
    sum() ==
    0
  control_augment <- if (output$settings$control_augment > 0) {
    control_augment_checks(output)
  } else {
    TRUE
  }
  random_assign <- if (output$settings$random_assign_prop > 0) {
    random_assign_checks(output)
  } else {
    TRUE
  }

  return(all(c(
    band_col_check,
    prob_col_check,
    est_check,
    anyNA_ests,
    control_augment,
    random_assign
  )))
}
multi_mab_checks <- function(output) {
  band_col_check <- length(output$settings$conditions) ==
    (ncol(output$bandits) - 2)
  prob_col_check <- length(output$settings$conditions) ==
    (ncol(output$assignment_probs) - 2)
  assignment_quant_check <- length(output$settings$conditions) ==
    (ncol(output$assignment_quantities) - 1)
  return(all(c(band_col_check, prob_col_check, assignment_quant_check)))
}
# Function to conduct the test
run_test <- function(full_args, static_args, trial) {
  FUN <- switch(
    trial,
    "single" = expression(single_mab_simulation),
    "multiple" = expression(multiple_mab_simulation)
  )
  class_specific_checks <- switch(
    trial,
    "single" = expression(single_mab_checks(output)),
    "multiple" = expression(multi_mab_checks(output))
  )
  class <- switch(trial, "single" = "mab", "multiple" = "multiple.mab")
  results <- purrr::map(seq_len(nrow(full_args)), \(x) {
    args <- c(as.list(full_args[x, ]), static_args)
    expect_no_failure({
      output <- do.call(eval(FUN), args)

      testthat::capture_output_lines(expect_no_failure(print(output)))
    })
    if (isTRUE(args$time_unit == "Month")) {
      args$data_cols <- args$data_cols[!names(args$data_cols) == "month_col"]
      expect_no_failure(do.call(eval(FUN), args))
    }
    expect_s3_class(output, class)
    expect_no_failure({
      if (!eval(class_specific_checks)) {
        stop("Post-Run Checks Failed")
      }
    })
    return(output)
  })

  purrr::walk(
    results,
    ~ {
      expect_no_failure(summary(.x))
      expect_no_failure(invisible(.x))
    }
  )
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    if (trial == "single") {
      types <- c("arm", "assign")
      purrr::walk(results, \(x) {
        purrr::walk(types, \(type) {
          expect_no_failure(plot(x, type = type))
        })
      })

      levels <- runif(3)
      purrr::walk(results, \(x) {
        purrr::walk(levels, \(level) {
          expect_no_failure(plot(x, type = "estimate", level = level))
        })
      })
    }
    if (trial == "multiple") {
      types <- c("hist", "estimate")
      cdfs <- c("normal", "empirical")
      quantities <- c("estimate", "assignment")

      purrr::walk(results, ~ expect_no_failure(plot(.x, type = "summary")))
      purrr::walk(
        results,
        ~ purrr::walk(quantities, \(y) {
          expect_no_failure(plot(.x, type = "hist", quantity = y))
        })
      )
      purrr::walk(
        results,
        ~ purrr::walk(cdfs, \(y) {
          expect_no_failure(plot(.x, type = "estimate", cdf = y))
        })
      )
    }
  }
}

equal_checks <- function(tbl, dt, type) {
  all_equals <- purrr::map2_lgl(
    tbl,
    dt,
    ~ {
      return(base::isTRUE(base::all.equal(.x, .y, check.attributes = FALSE)))
    }
  )
  return(all(all_equals))
}

single_equal_checks <- function(tbl_output, dt_output) {
  data.table::setorder(dt_output$estimates, estimator, mab_condition)
  tbl <- tbl_output[-1]
  dt <- dt_output[-1]
  check1 <- equal_checks(tbl, dt)
  summary_check <- base::isTRUE(base::all.equal(
    summary(tbl_output),
    summary(dt_output)
  ))
  return(all(check1, summary_check))
}
multi_equal_checks <- function(tbl_output, dt_output) {
  data.table::setorder(dt_output$estimates, trial, estimator, mab_condition)
  check1 <- equal_checks(tbl_output, dt_output)
  summary_check <- base::isTRUE(base::all.equal(
    summary(tbl_output),
    summary(dt_output)
  ))
  return(all(check1, summary_check))
}


check_dt_tibble_equal <- function(full_args, static_args, type, seed) {
  FUN <- switch(
    type,
    "single" = expression(single_mab_simulation),
    "multiple" = expression(multiple_mab_simulation)
  )
  class_equal_checks <- switch(
    type,
    "single" = expression(single_equal_checks(tbl_output, dt_output)),
    "multiple" = expression(multi_equal_checks(tbl_output, dt_output))
  )
  purrr::walk(seq_len(nrow(full_args)), \(x) {
    tbl_args <- c(as.list(full_args[x, ]), static_args)
    dt_args <- tbl_args
    dt_args$data <- data.table::data.table(dt_args$data)
    set.seed(seed)
    tbl_output <- do.call(eval(FUN), tbl_args)
    set.seed(seed)
    dt_output <- do.call(eval(FUN), dt_args)
    check_equal <- eval(class_equal_checks)
    expect_no_failure({
      if (!check_equal) {
        print(tbl_args)
        stop("Equality Checks Failed")
      }
    })
  })
}

# Potential Time Models
time_model_extended <- function(
  n,
  treatments,
  success,
  blocks = NULL,
  clusters = NULL
) {
  time <- data.table::fcase(
    treatments == "T1" , stats::rexp(n, rate = 5)                   ,
    treatments == "T2" , stats::rgamma(n, shape = 2, rate = 2)      ,
    treatments == "T3" , stats::rweibull(n, shape = 3, scale = 4)   ,
    treatments == "T4" , stats::rlnorm(n, meanlog = 1, sdlog = 0.5) ,
    treatments == "T5" , stats::runif(n, min = 0, max = 5)          ,
    treatments == "T6" , stats::rchisq(n, df = 4)
  )

  time <- dplyr::if_else(success == 1, round(time) * lubridate::days(3), NA)
  return(time)
}
time_model_alt <- function(
  n,
  treatments,
  success,
  blocks = NULL,
  clusters = NULL
) {
  arm_index <- base::as.numeric(substr(as.character(treatments), 2, 2))
  mu <- arm_index * 0.5 + 1

  time <- rnorm(n, mean = mu, sd = 1)
  time <- base::abs(time)

  time <- dplyr::if_else(
    success == 1,
    base::ceiling(time) * lubridate::days(7),
    NA
  )
  return(time)
}

# Generating Random Parameters

r_sum1 <- function(n) {
  x <- stats::runif(n, min = 0.4, max = 0.7)
  x / base::sum(x)
}

generate_random_params <- function(n) {
  n_arms <- base::sample(3:7, 1)
  n_blocks <- base::sample(c(0, 4), 1)
  n_clusters <- base::sample(c(0, 12), 1)

  has_blocks <- n_blocks > 0
  has_clusters <- n_clusters > 0

  blocks <- if (has_blocks) {
    stats::setNames(
      r_sum1(n_blocks),
      base::paste0("B", base::seq_len(n_blocks))
    )
  } else {
    NULL
  }

  clusters <- if (has_blocks && has_clusters) {
    cluster_list <- base::split(
      base::paste0("C", 1:n_clusters),
      base::cut(1:n_clusters, breaks = n_blocks, labels = FALSE)
    )

    stats::setNames(
      base::lapply(cluster_list, \(c) {
        stats::setNames(r_sum1(length(c)), c)
      }),
      base::paste0("B", base::seq_len(n_blocks))
    )
  } else if (has_clusters) {
    stats::setNames(
      r_sum1(n_clusters),
      base::paste0("C", base::seq_len(n_clusters))
    )
  } else {
    NULL
  }

  probs <- if (has_blocks && has_clusters) {
    stats::setNames(
      base::lapply(base::seq_len(n_arms), \(arm) {
        stats::setNames(
          base::lapply(base::names(clusters), \(b) {
            stats::setNames(
              stats::runif(base::length(clusters[[b]]), 0.4, 0.6),
              base::names(clusters[[b]])
            )
          }),
          base::names(clusters)
        )
      }),
      base::paste0("T", base::seq_len(n_arms))
    )
  } else if (has_blocks || has_clusters) {
    m <- max(n_blocks, n_clusters)
    letter <- c("B", "C")[which.max(c(n_blocks, n_clusters))]
    stats::setNames(
      base::lapply(base::seq_len(n_arms), \(arm) {
        stats::setNames(
          stats::runif(m, 0.4, 0.6),
          base::paste0(letter, base::seq_len(m))
        )
      }),
      base::paste0("T", base::seq_len(n_arms))
    )
  } else {
    stats::setNames(
      stats::runif(n_arms, 0.4, 0.6),
      base::paste0("T", base::seq_len(n_arms))
    )
  }

  return(base::list(
    n = n,
    dt = base::sample(c(TRUE, FALSE), 1),
    alg = base::sample(c("Thompson", "UCB1"), 1),
    time_model_fn = base::sample(
      base::list(time_model_extended, time_model_alt),
      1
    )[[1]],
    perfect_assignment = base::sample(c(TRUE, FALSE), 1),
    blocks = blocks,
    clusters = clusters,
    probs = probs
  ))
}

# Running Generate Data then mab
data_checks <- function(data, param) {
  df <- data |>
    dplyr::select(dplyr::any_of(c("treatment", "success", "block", "cluster")))
  check_relative_probs(df, param$probs)
  check_na(df)
}
check_na <- function(df) {
  x <- base::sum(base::is.na(df))
  if (x != 0) {
    stop("NA Present")
  }
}

check_relative_probs <- function(df, p) {
  p_real <- df |>
    dplyr::group_by(dplyr::across(-success)) |>
    dplyr::summarize(prob = base::mean(success), .groups = "drop")
  theory <- extract_success_prob(
    p = p,
    treatments = p_real$treatment,
    blocks = p_real$block,
    cluster = p_real$cluster
  )
  checks <- dplyr::near(theory, p_real$prob, tol = 0.25)

  if (!base::all(checks)) {
    base::stop("Probabilities diverge from user specified values")
  }
}


run_simulation <- function(params) {
  data <- generate_rct.bernoulli(
    n = params$n,
    p = params$probs,
    dt = params$dt,
    dates_of_assignment = lubridate::ymd("2023-01-01") +
      0:(6 - 1) * months(1),
    time_model = params$time_model_fn,
    blocks = params$blocks,
    clusters = params$clusters
  )
  expect_success(
    single_mab_simulation(
      data,
      assignment_method = "date",
      algorithm = params$alg,
      prior_periods = 2,
      perfect_assignment = params$perfect_assignment,
      whole_experiment = FALSE,
      blocking = (base::length(params$blocks) > 0),
      block_cols = c("block"),
      data_cols = c(
        success_col = "success",
        assignment_date_col = "assignment_date",
        success_date_col = "success_date",
        id_col = "id",
        condition_col = "treatment",
        date_col = "assignment_date"
      ),
      period_length = 1,
      time_unit = "month"
    )
  )
}

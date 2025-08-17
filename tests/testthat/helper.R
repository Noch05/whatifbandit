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
    treat_month = lubridate::month(date_of_treatment),
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
    )
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
      month_col = "treat_month"
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
        !(time_unit == "Month" & period_length > 1) &
        !(assignment_method == "Individual" & prior_periods != "All") &
        !(control_augment > 0 & random_assign_prop > 0)
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

# Function to conduct the test
#
run_test <- function(full_args, static_args, trial) {
  FUN <- switch(
    trial,
    "single" = expression(single_mab_simulation),
    "multiple" = expression(multiple_mab_simulation)
  )
  class <- switch(trial, "single" = "mab", "multiple" = "multiple.mab")

  results <- purrr::map(seq_len(nrow(full_args)), \(x) {
    args <- c(as.list(full_args[x, ]), static_args)
    expect_no_failure({
      output <- do.call(eval(FUN), args)
    })
    expect_s3_class(output, class)
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

      purrr::walk(results, ~ plot(.x, type = "summary"))
      purrr::walk(results, \(x) {
        expect_no_failure(plot(x, type = "hist"))
        purrr::walk(cdfs, \(z) {
          expect_no_failure(plot(x, type = "estimate", cdf = z))
        })
      })
    }
  }
}

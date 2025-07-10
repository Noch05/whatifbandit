test_that("Throws Proper Error when ID's are not unique", {
  data <- data.frame(
    id = rep(1, 10),
    success = rbinom(10, 1, .5),
    condition = sample(c(1, 2, 3), 10, replace = TRUE)
  )
  conditions <- as.character(unique(data$condition))

  expect_snapshot_error(single_mab_simulation(
    data = data,
    assignment_method = "Individual",
    whole_experiment = TRUE,
    blocking = FALSE,
    perfect_assignment = TRUE,
    algorithm = "Thompson",
    prior_periods = "All",
    conditions = conditions,
    data_cols = c(
      condition_col = "condition",
      id_col = "id",
      success_col = "success"
    ), verbose = TRUE
  ))
})

#-------------------------------------------------------------------------------

test_that("Throws Proper Error when arguments are invalid", {
  data <- tibble::tibble(
    id = seq(1, 10, 1),
    success = rbinom(10, 1, .5),
    condition = sample(c(1, 2, 3), 10, replace = TRUE),
    success_date = rep(lubridate::ymd("2025-01-01"), 10),
    assignment_date = rep(lubridate::ymd("2025-01-01"), 10),
    date = rep(lubridate::ymd("2024-01-01"), 10)
  )
  invalid_args_exs <- list(
    algorithm = list("not", 76),
    assignment_method = list("not", 45),
    verbose = list(456, "text"),
    control_augment = list(-1, 2, NA),
    blocking = list(654, "text"),
    time_unit = list("Weeks", 5),
    perfect_assignment = list(45, "text"),
    whole_experiment = list(546, "text"),
    prior_periods = list(-5, "text"),
    period_length = list(50, -1, "text"),
    conditions = list(c("1", "2", "3", "4"))
  )
  test_invalid_arg <- function(x, y) {
    args <- list(
      data = data,
      algorithm = "Thompson",
      assignment_method = "Batch",
      period_length = 1,
      verbose = TRUE,
      conditions = unique(as.character(data$condition)),
      control_augment = 0,
      blocking = FALSE,
      perfect_assignment = TRUE,
      whole_experiment = FALSE,
      data_cols = c(
        condition_col = "condition",
        id_col = "id",
        success_col = "success"
      ),
      prior_periods = "All"
    )
    if (y == "time_unit") {
      args$assignment_method <- "Date"
      args$data_cols <- c(
        condition_col = "condition",
        id_col = "id",
        success_col = "success",
        date_col = "date"
      )
    }
    if (y == "perfect_assignment") {
      args$data_cols <- c(
        condition_col = "condition",
        id_col = "id",
        success_col = "success",
        success_date_col = "success_date",
        assignment_date_col = "assignment_date"
      )
    }

    args[[y]] <- x

    expect_snapshot_error(do.call(single_mab_simulation, args))
  }
  purrr::walk2(invalid_args_exs, names(invalid_args_exs), \(x, y) {
    purrr::walk(x, ~ test_invalid_arg(.x, y))
  })
})
#-------------------------------------------------------------------------------

test_that("Columns that do not exist in data are found", {
  data <- tibble::tibble(
    id = seq(1, 10, 1),
    success = rbinom(10, 1, .5),
    condition = sample(c(1, 2, 3), 10, replace = TRUE),
  )

  col_args <- expand.grid(
    condition_col = c(condition_col = "condition", condition_col = "fake_colname"),
    id_col = c(id_col = "id", id_col = "fake_colname"),
    success_col = c(success_col = "success", success_col = "fake_colname"),
    stringsAsFactors = FALSE
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(fake_count = sum(dplyr::c_across(dplyr::everything()) == "fake_colname")) |>
    dplyr::filter(fake_count == 1) |>
    dplyr::ungroup() |>
    dplyr::select(-fake_count)

  col_args <- rbind(col_args, col_args)

  conditions <- as.character(unique(data$condition))

  purrr::walk(seq_len(nrow(col_args)), ~ {
    data_cols <- setNames(
      as.character(col_args[.x, ]),
      names(col_args)
    )
    if (.x > nrow(col_args) / 2) {
      ind <- .x - (nrow(col_args) / 2)
      data_cols <- data_cols[-ind]
    }
    expect_snapshot_error(single_mab_simulation(
      data = tanf,
      assignment_method = "Individual",
      whole_experiment = TRUE,
      blocking = FALSE,
      perfect_assignment = TRUE,
      algorithm = "Thompson",
      prior_periods = "All",
      conditions = conditions,
      data_cols = data_cols
    ))
  })
})
#-------------------------------------------------------------------------------

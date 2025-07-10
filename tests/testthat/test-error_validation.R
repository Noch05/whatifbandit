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
    algorithm = list("not", 76, NA),
    assignment_method = list("not", 45, NA),
    verbose = list(456, "text", NA),
    control_augment = list(-1, 2, NA, 0.5),
    blocking = list(654, "text", NA),
    time_unit = list("Weeks", 5),
    perfect_assignment = list(45, "text", NA),
    whole_experiment = list(546, "text", NA),
    prior_periods = list(-5, "text", NA),
    period_length = list(50, -1, "text", NA),
    conditions = list(c("1", "2", "3", "4"), rep(NA, 3))
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

test_that("Throws proper error when columns do not exist or not declared", {
  data <- tibble::tibble(
    id = seq(1, 10, 1),
    success = rbinom(10, 1, .5),
    condition = sample(c(1, 2, 3), 10, replace = TRUE),
    success_date = rep(lubridate::ymd("2025-01-01"), 10),
    assignment_date = rep(lubridate::ymd("2025-01-01"), 10),
    apt_date = rep(lubridate::ymd("2024-01-01"), 10),
    month = sample(c("June", "July"), replace = TRUE, size = 10),
    block = rbinom(10, 1, 0.3)
  )

  col_args <- expand.grid(
    block_cols = list(block_cols = c("block"), block_cols = c("block", "block2")),
    condition_col = c(condition_col = "condition", condition_col = "fake_colname"),
    id_col = c(id_col = "id", id_col = "fake_colname"),
    success_col = c(success_col = "success", success_col = "fake_colname"),
    success_date_col = c(success_date_col = "success_date", success_date_col = "fake_colname"),
    assignment_date_col = c(assignment_date_col = "assignment_date", assignment_date_col = "fake_colname"),
    date_col = c(date_col = "apt_date", date_col = "fake_colname"),
    month_col = c(month_col = "month", month_col = "fake_colname"),
    stringsAsFactors = FALSE
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(fake_count = sum(dplyr::c_across(dplyr::everything()) == "fake_colname")) |>
    dplyr::filter(fake_count <= 1 & any(block_cols != c("block", "block2"))) |>
    dplyr::ungroup() |>
    dplyr::select(-fake_count)
  col_args$block_cols[1] <- list(c("block", "block2"))
  col_args <- rbind(col_args, col_args)

  conditions <- as.character(unique(data$condition))

  purrr::walk(seq_len(nrow(col_args)), ~ {
    data_cols <- setNames(
      col_args[.x, ],
      names(col_args)
    )
    block_col <- eval(data_cols$block_cols[[1]])

    if (.x > (nrow(col_args) / 2)) {
      if (.x == (1 + (nrow(col_args) / 2))) {
        block_col <- NULL
      } else {
        data_cols <- data_cols[-(.x - nrow(col_args) / 2)]
      }
    }


    expect_snapshot_error(single_mab_simulation(
      data = data,
      assignment_method = "Date",
      time_unit = "Month",
      whole_experiment = TRUE,
      period_length = 1,
      blocking = TRUE,
      block_cols = block_col,
      perfect_assignment = FALSE,
      algorithm = "Thompson",
      prior_periods = "All",
      conditions = conditions,
      data_cols = data_cols[names(data_cols) != "block_cols"]
    ))
  })
})
#-------------------------------------------------------------------------------
test_that("Multiple Simulation Specific Error Checks work", {
  data <- tibble::tibble(
    id = seq(1, 10, 1),
    success = rbinom(10, 1, .5),
    condition = sample(c(1, 2, 3), 10, replace = TRUE),
    success_date = rep(lubridate::ymd("2025-01-01"), 10),
    assignment_date = rep(lubridate::ymd("2025-01-01"), 10),
    apt_date = rep(lubridate::ymd("2024-01-01"), 10),
    month = sample(c("June", "July"), replace = TRUE, size = 10),
    block = rbinom(10, 1, 0.3)
  )

  args_change <- list(
    times = list(-5, NA, "t"),
    seeds = list(rep(1, 5), rep("5", 5)),
    keep_data = list(NA, "text", 6543)
  )
  ##
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
    prior_periods = "All",
    times = 5,
    seeds = sample.int(1000, 5),
    keep_data = FALSE
  )

  purrr::walk2(args_change, names(args_change), \(x, y){
    purrr::walk(x, ~ {
      args[[y]] <- .x
      expect_snapshot_error(do.call(multiple_mab_simulation, args))
    })
  })
})

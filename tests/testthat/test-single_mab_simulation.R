test_that("Throws Proper Error when ID's are not unique", {
  data(tanf)
  tanf <- tanf[sample.int(15, 100, replace = TRUE), ]
  conditions <- setNames(
    c("no_letter", "open_appt", "specific_appt"),
    c("Control", "T1", "T2")
  )
  expect_snapshot_error(single_mab_simulation(
    data = tanf,
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


test_that("Columns that do not exist in data are found", {
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
    select(-fake_count)

  purrr::walk(seq_len(nrow(col_args)), ~ {
    data_cols <- setNames(
      as.character(col_args[.x, ]),
      names(col_args)
    )

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

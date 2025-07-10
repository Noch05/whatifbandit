test_that("Throws Proper Error when ID's are not unique", {
  data(tanf)
  conditions <- setNames(
    c("no_letter", "open_appt", "specific_appt"),
    c("Control", "T1", "T2")
  )
  expect_error(single_mab_simulation(
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
      success_col = "success",
    ), verbose = TRUE
  ))
})
# Making Id's Unique
test_that("Column Checking Works Properly", {


})

test_that("BaseR/Dplyr datatype test", {
  set.seed(43243)
  required_inputs <- generate_data(100, 3)
  full_args <- required_inputs$full_args
  static_args <- required_inputs$static_args
  static_args$data$type_of_policy <- as.factor(static_args$data$type_of_policy)
  run_test(full_args, static_args, trial = "single")
  static_args$data$type_of_policy <- as.numeric(static_args$data$type_of_policy)
  static_args$control_condition <- 1
  run_test(full_args, static_args, trial = "single")
})

test_that("Data.table datatype test", {
  set.seed(43243)
  required_inputs <- generate_data(100, 3)
  full_args <- required_inputs$full_args
  static_args <- required_inputs$static_args
  static_args$data <- data.table(static_args$data)
  static_args$data$type_of_policy <- as.factor(static_args$data$type_of_policy)
  run_test(full_args, static_args, trial = "single")
  static_args$data$type_of_policy <- as.numeric(static_args$data$type_of_policy)
  static_args$control_condition <- 1
  run_test(full_args, static_args, trial = "single")
})

test_that("BaseR/Dplyr Single Simulation Test", {
  set.seed(5832601)
  required_inputs <- generate_data(100, 3)
  full_args <- required_inputs$full_args
  static_args <- required_inputs$static_args
  run_test(full_args, static_args, trial = "single")
})

test_that("data.table Single Simulation Test", {
  set.seed(1235493)
  required_inputs <- generate_data(100, 3)
  full_args <- required_inputs$full_args
  static_args <- required_inputs$static_args
  static_args$data <- data.table::setDT(static_args$data)

  run_test(full_args, static_args, trial = "single")
})

test_that("BaseR/Dplyr Multiple Simulation Test", {
  set.seed(043932)
  required_inputs <- generate_data(100, 3)
  full_args <- required_inputs$full_args
  static_args <- required_inputs$static_args
  static_args$times <- 2
  static_args$seeds <- sample.int(100, 2)
  static_args$keep_data <- FALSE

  run_test(full_args, static_args, trial = "multiple")
})

test_that("data.table Multiple Simulation Test", {
  set.seed(494873)
  required_inputs <- generate_data(100, 3)
  full_args <- required_inputs$full_args
  static_args <- required_inputs$static_args
  static_args$times <- 2
  static_args$seeds <- sample.int(100, 2)
  static_args$keep_data <- FALSE
  static_args$data <- data.table::setDT(static_args$data)

  run_test(full_args, static_args, trial = "multiple")
})

test_that("BaseR/Dplyr Single Simulation Test", {
  ## Using strange Names to Replicate potential names
  ## in a real data set
  set.seed(5832601)
  required_inputs <- generate_data(100)
  full_args <- required_inputs$full_args
  static_args <- required_inputs$static_args
  run_test(full_args, static_args)
})

test_that("data.table Single Simulation Test", {
  set.seed(1235493)
  required_inputs <- generate_data(100)
  full_args <- required_inputs$full_args
  static_args <- required_inputs$static_args
  static_args$data <- data.table::setDT(static_args$data)
  run_test(full_args, static_args)
})

test_that("BaseR/Dplyr Single Simulation Test", {
  ## Using strange Names to Replicate potential names
  ## in a real data set
  set.seed(5832601)
  required_inputs <- generate_data(100)
  full_args <- required_inputs$full_args
  static_args <- required_inputs$static_args
  run_test(full_args, static_args)
})

test_that("data.table Single Simulation Test", {
  set.seed(1235493)
  required_inputs <- generate_data(100)
  full_args <- required_inputs$full_args
  static_args <- required_inputs$static_args
  static_args$data <- data.table::setDT(static_args$data)
  run_test(full_args, static_args)
})

test_that("Single Sim Equality Check", {
  set.seed(123401)
  required_inputs <- generate_data(100, 3)
  full_args <- required_inputs$full_args
  static_args <- required_inputs$static_args

  check_dt_tibble_equal(
    static_args = static_args,
    full_args = full_args,
    type = "single",
    seed = 658454
  )
})

test_that("Multi Sim Equality check", {
  set.seed(971834)
  required_inputs <- generate_data(100, 3)
  full_args <- required_inputs$full_args
  static_args <- required_inputs$static_args
  static_args$times <- 2
  static_args$seeds <- sample.int(100, 2)

  check_dt_tibble_equal(
    static_args = static_args,
    full_args = full_args,
    type = "multiple",
    seed = 5894782
  )
})

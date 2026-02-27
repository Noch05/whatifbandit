test_that("Generate data test with single mab", {
  set.seed(39489)
  purrr::walk(1:25, \(x) {
    params <- generate_random_params(1000)
    run_simulation(params)
  })
})

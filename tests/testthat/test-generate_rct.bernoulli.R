test_that("Error Validation on for `generate_rct.bernoulli`", {
  set.seed(5)
  tests <- list(rnorm(10, mean = 0, sd = 0.05), rgamma(10, 100))
    purrr::walk(tests, \(p) {
      expect_snapshot_error(
      generate_rct.bernoulli(n = 100, arms = 4, p = p)
      )})
    })
   

test_that("Generate data test with single mab", {
    set.seed(39434954)
    purrr::walk(1:20, \(x) {
      params <- generate_random_params()
      expect_no_error(
        run_simulation(params)
      )
    }) 
})

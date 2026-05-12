test_that("Formula Parsing", {
  form <- list(
    form1 = s ~ c + block(a, d) + cluster(g),
    form2 = s ~ a + block(b),
    form3 = s ~ a + cluster(p),
    form4 = s ~ c + d
  )

  truth <- list(
    list(
      condition_col = "c",
      success_col = "s",
      block_cols = c("a", "d"),
      cluster_col = "g"
    ),
    list(
      condition_col = "a",
      success_col = "s",
      block_cols = "b",
      cluster_col = NULL
    ),
    list(
      condition_col = "a",
      success_col = "s",
      block_cols = NULL,
      cluster_col = "p"
    ),
    list(
      condition_col = "c",
      success_col = "s",
      block_cols = NULL,
      cluster_col = NULL
    )
  )
  purrr::walk2(form, truth, \(f, t) {
    expect_equal(formula_parse(f), t)
  })
})

test_that("Finalize Prior List", {
  test_list <- list(
    mab_condition = c("b", "a"),
    n = c(5, 10),
    successes = c(3, 7)
  )
  conditions <- c("d", "a", "b", "c")

  truth <- list(
    n = c(a = 10, b = 5, c = 0, d = 0),
    successes = c(a = 7, b = 3, c = 0, d = 0)
  )

  expect_equal(finalize_prior_list(test_list, conditions = conditions), truth)
})

test_that("Invalid Bandits", {
  expect_false(bandit_invalid(c(0, 0.4, 0.3)))
  expect_true(bandit_invalid(c(NA, 0.4, 0.3)))
  expect_true(bandit_invalid(c(2e-16, 2e-16, 0)))
})


test_that("Input Check Helpers", {
  test_funcs <- list(
    check_logical,
    check_prop,
    check_posint,
    check_sum1,
    check_string,
    check_names
  )
  passes <- list(
    list(TRUE, FALSE),
    list(
      control_augment = 0.5,
      control_augment = 0.99,
      random_assign_prop = 0.1
    ),
    list(5, 10, 200),
    list(c(0, 0, 1), c(0.1, 0.5, 0.4), c(0.2, 0.3, 0.5)),
    list(list(arg = "bea", valid = "bea", name = "b")),
    list(c(b = 5, c = 1), c(y = 6))
  )
  fails <- list(
    list(5, -5, NA, "b"),
    list(-1, 100, NA, "s"),
    list(-5, 0, "g", 5.5),
    list(c(1, 1, 1), c(3, 3, 2)),
    list(
      list(arg = 5, valid = "b", name = "beans"),
      list(arg = "bea", valid = "i", name = "beans")
    ),
    list(c(5, 3, 2), c("Bea", "d"))
  )
  purrr::pwalk(list(test_funcs, passes, fails), \(t, p, f) {
    input_check_test(t, p, f)
  })
})


test_that("Summary to Matrix", {
  set.seed(5)
  fail <- runif(10)
  success <- runif(10)
  treatment_block <- paste0("T", 1:10)

  df <- data.frame(
    failure_rate = fail,
    success_rate = success,
    random = rbinom(1, 10, 0.3),
    random2 = sample(10),
    treatment_block = treatment_block
  )
  expect_equal(
    summary_to_matrix(df),
    matrix(
      c(fail, success),
      ncol = 2,
      nrow = 10,
      dimnames = list(treatment_block, c("failure_rate", "success_rate"))
    )
  )
})


test_that("Period Sizes", {
  data <- data.frame(period_number = c(1, 1, 1, 2, 2, 2, 3, 3, 4, 5))
  truth <- c(3, 3, 2, 1, 1)

  compute_period_sizes(data) |> expect_equal(truth)
  compute_period_sizes(data.table::as.data.table(data)) |>
    expect_equal(truth)
})

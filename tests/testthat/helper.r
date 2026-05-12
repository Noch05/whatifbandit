input_check_test <- function(test, pass, fails) {
  purrr::walk(pass, \(p) {
    p <- if (is.list(p)) p else list(p)
    expect_no_error(do.call(test, p))
  })
  purrr::walk(fails, \(f) {
    f <- if (is.list(f)) f else list(f)
    expect_snapshot_error(do.call(test, f))
  })
}

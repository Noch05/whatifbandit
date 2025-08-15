generate_rct_bernoulli <- function(N, arms, random, block, cluster, probs) {
  id <- seq_len(N)
  treatment <- get_treatments(N = N, arms = arms, random = random, block = block, cluster = cluster)
  Y <- rbinom(N, 1, probs[treatment + 1])
  return(data.frame(id, treatment, Y))
}

get_treatments <- function(N, arms, random, block, cluster) {
  return(rbinom(N, 1, 0.5))
}



data_create <- function(N, arms,) {
  id <- seq_len(N)
  treatment <- randomizr::complete_ra(N = N, num_arms = arms, conditions = seq_len(arms))
  return(tibble::tibble(id, treatment))
}


x <- sample(c(1,2,3, 4, 5, 6, 7), 10000, replace = TRUE)

microbenchmark::microbenchmark(
  base = {y <- dplyr::case_when(
    x == 1 ~ 5,
    x == 2 ~ 3,
    x == 3 ~ 4,
    x == 4 ~ 4,
    x == 5 ~ 8, 
    x == 7 ~ rbinom(10000, 1, 0.5)
  )},
  dta = { y <- data.table::fcase(
    x == 1, 5, x == 2, 3, x == 3, 4, x == 4, 5, x == 5, 1002, x == 7, as.double(rbinom(10000, 1, .2))
  )}
)

# 4-Arm Design;

generate_rct.bernoulli(n = 1000, p = c(0.3, 0.7, 0.5, 0.4))

# Blocked and Clustered with random probabilities

generate_rct.bernoulli(
  n = 1000,
  p = list(
    Control = list(
      B1 = c(C1 = runif(3), C2 = runif(3)),
      B2 = c(C3 = runif(3), C4 = runif(3))
    ),
    T1 = list(
      B1 = c(C1 = runif(3), C2 = runif(3)),
      B2 = c(C3 = runif(3), C4 = runif(3))
    ),
    T2 = list(
      B1 = c(C1 = runif(3), C2 = runif(3)),
      B2 = c(C2 = runif(3), C3 = runif(3))
    )
  ),
  blocks = c(B1 = 0.3, B2 = 0.7),
  clusters = list(B1 = c(C1 = 0.3, C2 = 0.7), B2 = c(C3 = 0.2, C4 = 0.8))
)

# Modelling Success Dates

time_model <- function(n, treatments, success, blocks, clusters = NULL) {
  # Specific model for each treatment and block
  time <- data.table::fcase(
    treatments == "T1" & blocks == "B1" , rexp(n, rate = 5)                 ,
    treatments == "T1" & blocks == "B2" , rexp(n, rate = 10)                ,
    treatments == "T2" & blocks == "B1" , rgamma(n, shape = 2, rate = 2)    ,
    treatments == "T2" & blocks == "B2" , rweibull(n, shape = 3, scale = 4)
  )

  time <- ifelse(success == 1, round(time) * lubridate::days(1), NA)
  return(time)
}
# Using with `single_mab_simulation()`

set.seed(100)
result <- generate_rct.bernoulli(
  n = 10000,
  p = list(
    T1 = c(B1 = 0.5, B2 = 0.3, B3 = 0.2),
    T2 = c(B1 = 0.7, B2 = 0.6, B3 = 0.9)
  ),
  blocks = c(B1 = 0.3, B2 = 0.4, B3 = 0.3),
  dates_of_assignment = lubridate::ymd("2023-04-15") +
    0:24 * months(1),
  time_model = time_model
) |>
  single_mab_simulation(
    assignment_method = "date",
    time_unit = "month",
    period_length = 1,
    algorithm = "Thompson",
    prior_periods = "All",
    perfect_assignment = FALSE,
    whole_experiment = FALSE,
    blocking = FALSE,
    data_cols = c(
      id_col = "id",
      success_col = "success",
      condition_col = "treatment",
      date_col = "assignment_date",
      assignment_date_col = "assignment_date",
      success_date_col = "success_date"
    )
  )
print(result)
result$estimates[1:3, ]

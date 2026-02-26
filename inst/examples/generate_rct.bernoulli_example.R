# 2-Arm Design; Complete Randomization

generate_rct.bernoulli(n = 1000, p = c(0.3, 0.7), simple = FALSE)

# Linear relationships in treatment

generate_rct.bernoulli(n = 1000, p = (1:10) * 0.1 - 0.05)

# Modelling Success Dates

time_model <- function(n, t, s) {
  # Specific times for each treatment
  time <- data.table::fcase(
    t == "T1" , rexp(n, rate = 5)                 ,
    t == "T2" , rgamma(n, shape = 2, rate = 2)    ,
    t == "T3" , rweibull(n, shape = 3, scale = 4)
  )

  time <- ifelse(s == 1, round(time) * lubridate::days(3), NA)
  return(time)
}
# Using with `single_mab_simulation()`

set.seed(100)
result <- generate_rct.bernoulli(
  n = 10000,
  p = c(0.3, 0.6, 0.45),
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

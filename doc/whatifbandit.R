## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(data.table)
library(dplyr)
library(future)
library(ggplot2)
library(whatifbandit)
set.seed(34934501)

## ----data---------------------------------------------------------------------
data(tanf)
glimpse(tanf)

## ----singlesimnoeval, eval=FALSE----------------------------------------------
# sim <- single_mab_simulation(
#   data = tanf,
#   assignment_method = "Batch",
#   period_length = 1000,
#   algorithm = "UCB1",
#   whole_experiment = TRUE, perfect_assignment = TRUE,
#   prior_periods = "All",
#   blocking = FALSE,
#   data_cols = c(
#     id_col = "ic_case_id",
#     success_col = "success",
#     condition_col = "condition"
#   )
# )

## ----singlesimeval------------------------------------------------------------
sim <- single_mab_simulation(
  data = tanf,
  assignment_method = "Date",
  time_unit = "Month",
  period_length = 1,
  algorithm = "Thompson",
  whole_experiment = FALSE, perfect_assignment = TRUE,
  prior_periods = "All",
  blocking = TRUE, block_cols = c("service_center"),
  data_cols = c(
    id_col = "ic_case_id",
    date_col = "appt_date",
    success_col = "success",
    condition_col = "condition",
    month_col = "recert_month",
    success_date_col = "date_of_recert",
    assignment_date_col = "letter_sent_date"
  )
)

## ----singlesimcontrolaug, eval=FALSE------------------------------------------
# tanf <- arrange(tanf, appt_date)
# conditions <- setNames(levels(tanf$condition), c("Control", "T1", "T2"))
# 
# sim <- single_mab_simulation(
#   data = tanf,
#   assignment_method = "Individual",
#   algorithm = "Thompson",
#   whole_experiment = FALSE, perfect_assignment = TRUE,
#   prior_periods = 500,
#   blocking = TRUE, block_cols = c("service_center"),
#   data_cols = c(
#     id_col = "ic_case_id",
#     success_col = "success",
#     condition_col = "condition"
#   ),
#   control_augment = 0.2,
#   control_condition = "no_letter"
# )

## -----------------------------------------------------------------------------
class(sim)
sim

## -----------------------------------------------------------------------------
sim_summary <- summary(sim)
print(sim_summary, width = Inf)

## -----------------------------------------------------------------------------
# Inside Summary Call
summary(sim, level = 0.8) |>
  select(estimated_probability_of_success, SE, lower_bound, upper_bound, level) |>
  print(width = Inf)

# By hand
quantile <- qnorm(0.2 / 2, lower.tail = FALSE)
sim_summary |>
  mutate(
    lower_bound = estimated_probability_of_success - SE * quantile,
    upper_bound = estimated_probability_of_success + SE * quantile
  ) |>
  select(estimated_probability_of_success, SE, lower_bound, upper_bound)

## -----------------------------------------------------------------------------
plot(sim, type = "arm")

## -----------------------------------------------------------------------------
plot(sim, type = "assign")

## -----------------------------------------------------------------------------
plot(sim, type = "estimate", level = 0.9, height = 0.4) +
  scale_x_continuous(breaks = seq(0, 1, .1), limits = range(0, 1))

## -----------------------------------------------------------------------------
set.seed(532454)
seeds <- sample.int(1000000, 100, replace = FALSE)

time <- system.time(
  multiple_sims <- multiple_mab_simulation(
    data = tanf,
    assignment_method = "Date",
    time_unit = "Month",
    period_length = 1,
    algorithm = "Thompson",
    whole_experiment = FALSE, perfect_assignment = TRUE,
    prior_periods = "All",
    blocking = TRUE, block_cols = c("service_center"),
    data_cols = c(
      id_col = "ic_case_id",
      date_col = "appt_date",
      success_col = "success",
      condition_col = "condition",
      month_col = "recert_month",
      success_date_col = "date_of_recert",
      assignment_date_col = "letter_sent_date"
    ),
    keep_data = TRUE, times = 100, seeds = seeds
  )
)

## -----------------------------------------------------------------------------
get_size <- function(x, unit) {
  string_format <- object.size(x) |> format(units = unit)
  num <- regmatches(string_format, regexpr("[0-9]+", string_format)) |> as.numeric()
  return(num)
}
full_size <- get_size(multiple_sims, unit = "MiB")
full_size_kib <- get_size(multiple_sims, unit = "KiB")
multiple_sims$final_data_nest <- NULL
reduced_size <- get_size(multiple_sims, unit = "KiB")


## -----------------------------------------------------------------------------
load("parallel.RData")

## -----------------------------------------------------------------------------
# set.seed(532454)
# seeds <- sample.int(1000000, 100, replace = FALSE)
# plan("multisession", workers = 4)
# parallel_time <- system.time(
#   multiple_sims <- multiple_mab_simulation(
#     data = tanf,
#     assignment_method = "Date",
#     time_unit = "Month",
#     period_length = 1,
#     algorithm = "Thompson",
#     whole_experiment = FALSE, perfect_assignment = TRUE,
#     prior_periods = "All",
#     blocking = TRUE, block_cols = c("service_center"),
#     data_cols = c(
#       id_col = "ic_case_id",
#       date_col = "appt_date",
#       success_col = "success",
#       condition_col = "condition",
#       month_col = "recert_month",
#       success_date_col = "date_of_recert",
#       assignment_date_col = "letter_sent_date"
#     ),
#     keep_data = FALSE, times = 100, seeds = seeds
#   )
# )
# plan("sequential")

## -----------------------------------------------------------------------------
class(multiple_sims)
print(multiple_sims)

## -----------------------------------------------------------------------------
summary(multiple_sims) |> print(width = Inf)

## -----------------------------------------------------------------------------
summary(multiple_sims, level = 0.75) |> print(width = Inf)

## -----------------------------------------------------------------------------
plot(multiple_sims, type = "summary")

## -----------------------------------------------------------------------------
plot(multiple_sims, type = "hist", quantity = "estimate", geom = list(bins = 50))
plot(multiple_sims, type = "hist", quantity = "assignment", facet = list(switch = "x"))

## -----------------------------------------------------------------------------
plot(multiple_sims, type = "estimate",
  cdf = "empirical", level = 0.99, height = 0.4
) +
  scale_x_continuous(breaks = seq(0, 1, .1), limits = range(0, 1))

## -----------------------------------------------------------------------------
load("datatable.RData")

## -----------------------------------------------------------------------------
# # Prepare the dataset:
# tanf_large <- tanf
# setDT(tanf_large)
# for (i in 1:9) {
#   tanf_large <- rbindlist(list(tanf_large, tanf_large))
# }
# 
# setorder(tanf_large, appt_date)
# 
# # Set id to be the row number for uniqueness:
# tanf_large[, id := .I]

## -----------------------------------------------------------------------------
# set.seed(523432453)
# dataframe_time <- system.time(single_mab_simulation(
#   data = as.data.frame(tanf_large),
#   assignment_method = "Batch",
#   period_length = 3000,
#   algorithm = "Thomspon",
#   whole_experiment = FALSE, perfect_assignment = TRUE,
#   prior_periods = "All",
#   blocking = FALSE,
#   data_cols = c(
#     id_col = "id",
#     success_col = "success",
#     condition_col = "condition"
#   ),
#   ndraws = 5000
# ))
# 
# datatable_time <- system.time(single_mab_simulation(
#   data = tanf_large,
#   assignment_method = "Batch",
#   period_length = 3000,
#   algorithm = "UCB1",
#   whole_experiment = FALSE, perfect_assignment = TRUE,
#   prior_periods = "All",
#   blocking = FALSE,
#   data_cols = c(
#     id_col = "id",
#     success_col = "success",
#     condition_col = "condition"
#   ),
#   ndraws = 5000
# ))


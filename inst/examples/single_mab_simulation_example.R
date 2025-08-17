# Loading Example Data and defining conditions
data(tanf)

## Running Thompson sampling with 500 person large batches,
## with no blocks and imperfect assignment

single_mab_simulation(
  data = tanf,
  assignment_method = "Batch",
  algorithm = "Thompson",
  period_length = 500,
  prior_periods = "All",
  blocking = FALSE,
  whole_experiment = TRUE,
  perfect_assignment = FALSE,
  data_cols = c(
    condition_col = "condition",
    id_col = "ic_case_id",
    success_col = "success",
    success_date_col = "date_of_recert",
    assignment_date_col = "letter_sent_date"
  )
)

## Running UCB1 Sampling with 1 Month based batches and
## control augmentation set to 0.25, with perfect_assignment.
## When using control_augment > 0, conditions need to have proper names
names(conditions) <- c("Control", "T1", "T2")
# no_letter is control, the others are treatments

single_mab_simulation(
  data = tanf,
  assignment_method = "Date",
  time_unit = "Month",
  algorithm = "UCB1",
  period_length = 1,
  prior_periods = "All",
  blocking = FALSE,
  whole_experiment = TRUE,
  perfect_assignment = TRUE,
  control_condition = "no_letter",
  control_augment = 0.25,
  data_cols = c(
    condition_col = "condition",
    id_col = "ic_case_id",
    success_col = "success",
    date_col = "appt_date",
    month_col = "recert_month"
  )
)

## 5 Day Periods with Thompson, Treatment Blocking by Service Center,
## Whole experiment FALSE, and hybrid assignment 10% random, 90% bandit.
single_mab_simulation(
  data = tanf,
  assignment_method = "Date",
  time_unit = "Day",
  algorithm = "Thompson",
  period_length = 5,
  prior_periods = "All",
  blocking = TRUE,
  block_cols = c("service_center"),
  whole_experiment = TRUE,
  perfect_assignment = TRUE,
  random_assign_prop = 0.1,
  data_cols = c(
    condition_col = "condition",
    id_col = "ic_case_id",
    success_col = "success",
    date_col = "appt_date"
  )
)

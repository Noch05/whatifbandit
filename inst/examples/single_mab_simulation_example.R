 # Loading Example Data and defining conditions
 data(tanf)
 conditions <- c("no_letter", "open_appt", "specific_appt")

 ## Running Thompson Sampling with 500 person large batches,
 ## with no blocks and imperfect assignment

 single_mab_simulation(
   data = tanf,
   assignment_method = "Batch",
   algorithm = "Thompson",
   period_length = 500,
   prior_periods = "All",
   blocking = FALSE,
   whole_experiment = TRUE,
   conditions = conditions,
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
   conditions = conditions,
   control_augment = 0.25,
   data_cols = c(
     condition_col = "condition",
     id_col = "ic_case_id",
     success_col = "success",
     date_col = "appt_date",
     month_col = "recert_month"
   )
 )

 ## If you misspecify or miss an argument, an appropriate error will be given
 ## I specified Month assignment but did not provide a month_column in my data

 try(single_mab_simulation(
   data = tanf,
   assignment_method = "Date",
   time_unit = "Month",
   algorithm = "UCB1",
   period_length = 1,
   prior_periods = "All",
   blocking = FALSE,
   whole_experiment = TRUE,
   perfect_assignment = FALSE,
   conditions = conditions,
   data_cols = c(
     condition_col = "condition",
     id_col = "ic_case_id",
     success_col = "success",
     date_col = "appt_date"
   )
 ))

 # I specified a negative period_length

 try(single_mab_simulation(
   data = tanf,
   assignment_method = "Date",
   time_unit = "Month",
   algorithm = "UCB1",
   period_length = -500,
   prior_periods = "All",
   blocking = FALSE,
   whole_experiment = TRUE,
   perfect_assignment = FALSE,
   conditions = conditions,
   control_augment = 0,
   data_cols = c(
     condition_col = "condition",
     id_col = "ic_case_id",
     success_col = "success",
     date_col = "appt_date"
   )
 ))

 # I forgot to add column containing the successes of the original experiment

 try(single_mab_simulation(
   data = tanf,
   assignment_method = "Batch",
   algorithm = "Thompson",
   period_length = 500,
   prior_periods = "All",
   blocking = FALSE,
   whole_experiment = TRUE,
   perfect_assignment = TRUE,
   conditions = conditions,
   control_augment = 0,
   data_cols = c(
     condition_col = "condition",
     id_col = "ic_case_id"
   )
 ))

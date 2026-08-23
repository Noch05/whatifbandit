library(whatifbandit)
library(tidyverse)
data(tanf)

future::plan("multisession", workers = 2)
set.seed(53254)
tanf_simulations <- mab_from_rct(
  success ~ condition + block(service_center),
  data = tanf,
  algorithm = "thompson",
  period_method = "date",
  time_unit = "month",
  period_length = 1,
  delayed_feedback = TRUE,
  assignment_date_col = letter_sent_date,
  success_date_col = date_of_recert,
  date_col = appt_date,
  month_col = recert_month,
  whole_experiment = FALSE,
  random_assign_prop = 0.3,
  r = 100,
  keep_data = TRUE,
  seed = TRUE
)
future::plan("sequential")

save.image("vignettes/tanf_sims.RData")

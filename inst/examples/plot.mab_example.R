# Objects returned by `single_mab_simulation()` have a `mab` class.
# This comes a plot generic that has several useful, minimal plots

# Loading Data and running a quick simulation
data(tanf)
x <- single_mab_simulation(
  data = tanf,
  algorithm = "Thompson",
  assignment_method = "Batch",
  period_length = 600,
  whole_experiment = TRUE,
  perfect_assignment = TRUE,
  blocking = FALSE,
  prior_periods = "All",
  conditions = c(
    "no_letter",
    "open_appt",
    "specific_appt"
  ),
  data_cols = c(
    condition_col = "condition",
    id_col = "id",
    success_col = "success"
  )
)
# We can plot the Thompson Probabilities over all periods.
# Using "assign" is also an option but that is only different
# when control augmentation is used
y <- plot(x, type = "arm")
y
# These can be added to like any ggplot2 object
library(ggplot2)
y + labs(title = "Your New Title")

# We can also plot the Augmented Inverse Probability Estimates.
# by default it provides 95% Normal Confidence Intervals but this can be adjusted
plot(x, type = "estimate", estimator = "AIPW")

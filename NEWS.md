# whatifbandit 0.2.1
* Submission to CRAN
  
## New Features
* `multiple_mab_simulation()` calculates the number of observations assigned to each treatment for each trial, and provides support for plotting them
has been added to `plot.multiple.mab()` via the `type = "hist"` and `quantity = "assignment"` arguments.
* `summary.mab()` now includes a new column with the number of observations assigned to each treatment.
* `summary.multiple.mab()` now includes two new columns with the mean and standard deviation for the number of observations assigned to each
treatment across the simulations.
* Month-based assignment, `time_unit = "month"` can now be specified with and without an appropriate `month_col`, resulting 
in either time-based (no `month_col`) or calendar-based (provided `month_col`) assignments. See the `time_unit` documentation for
more details.

## Fixes and Improvements
* Fixed handling of numeric and factor types in `condition_col` of the `data_cols` argument.
* Weighting AIPW by group size along with adaptive weights.
* Fixed inconsistent results across with data.frames, tibbles, and data.tables. Running `single_mab_simulation()` or `multiple_mab_simulation()`, with
the same seeds on the same system, now results in the same outcome regardless of input data class.
* `plot.multiple.mab()` now accepts arguments for `ggplot2::facet_grid()` for more precise customizations.
* `plot.mab()` for `type = "assign"` now displays the proportion of total observations assigned to each treatment for each period, instead of the individual probability of assignment. 


# whatifbandit 0.2.0

## New Features
* `multiple_mab_simulation()` supports parallel processing via [future](https://future.futureverse.org/).
* `single_mab_simulation()` and `multiple_mab_simulation()` support
[data.table](https://cran.r-project.org/package=data.table) for larger data sets.
* `summary()`, `print()`, and `plot()` generics for `mab` and `multiple.mab` class objects.
* `single_mab_simulation()` and `multiple_mab_simulation()` throw informative
error messages, relating to argument specification, and data types passed.

## Fixes and Improvements
* Fixed AIPW calculations mistakes.
* Fixed improper random seeding in `multiple_mab_simulation()`.
* Improved numerical calculation errors in Thompson sampling with large datasets.
* Optimization reduced simulation runtime by up to 50%.


# whatifbandit 0.1.0
* `single_mab_simulation()` and `multiple_mab_simulation()` simulate successfully.

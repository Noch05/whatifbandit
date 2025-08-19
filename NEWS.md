# whatifbandit 0.2.1

## Fixes and Improvements
* Fixed handling of numeric and factor types in `conditions_col` of the `data_cols` argument.
* Weighting AIPW by group size along with adaptive weights.
* Fixed inconsistent results across with data.frames, tibbles, and data.tables. Running `single_mab_simulation()` or `multiple_mab_simulation()`, with
the same seeds on the same system, now results in the same outcome regardless of input data class.

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

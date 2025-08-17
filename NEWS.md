
# whatifbandit 0.2.0
* Initial CRAN Submission

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

# whatifbandit 0.2.0

* Initial CRAN submission

# whatifbandit 0.1.3

## New Features
* Robust type checking for data passed into `single_mab_simulation()`
and `multiple_mab_simulation()` to throw more informative errors.

## Fixes
* Improved numerical instability issues in Thompson Sampling Calculations with large datasets.
* Further optimization reducing run time of simulations.

# whatifbandit 0.1.2

## New Features
* `single_mab_simulation()` and `multiple_mab_simulation()` support
[data.table](https://cran.r-project.org/package=data.table) for larger data sets.
* `summary()`, `print()`, and `plot()` generics for `mab` and `multiple.mab` class objects.

## Fixes
* `single_mab_simulation()` and `multiple_mab_simulation()` optimized decreasing
run time by up to 50%.
* Fixed AIPW Calculation Error.
* Fixed improper seeding of random state in `multiple_mab_simulation()`.

# whatifbandit 0.1.1

## New Features
* `multiple_mab_simulation()` supports parallel processing via [future](https://future.futureverse.org/).
* `single_mab_simulation()` and `multiple_mab_simulation()` throw informative
error messages.

# whatifbandit 0.1.0
* `single_mab_simulation()` and `multiple_mab_simulation()` simulate successfully.

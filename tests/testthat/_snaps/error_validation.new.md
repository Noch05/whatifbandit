# Throws Proper Error when ID's are not unique

    id is not a unique identifier; a unique ID for each observation is required.

# Throws Proper Error when arguments are invalid

    'algorithm' must be 'thompson' or 'ucb1'.
    x You passed: not

---

    'algorithm' must be 'thompson' or 'ucb1'.
    x You passed: 76

---

    'algorithm' must be 'thompson' or 'ucb1'.
    x You passed: NA

---

    Invalid `assignment_method`
    x you passed: not
    i Valid methods are `individual`, `batch`, `date`

---

    Invalid `assignment_method`
    x you passed: 45
    i Valid methods are `individual`, `batch`, `date`

---

    Invalid `assignment_method`
    x you passed: NA
    i Valid methods are `individual`, `batch`, `date`

---

    i In index: 1.
    i With name: verbose.
    Caused by error in `.f()`:
    ! `verbose` must be a logical (TRUE or FALSE)
    x You Passed: 456

---

    i In index: 1.
    i With name: verbose.
    Caused by error in `.f()`:
    ! `verbose` must be a logical (TRUE or FALSE)
    x You Passed: text

---

    i In index: 1.
    i With name: verbose.
    Caused by error in `.f()`:
    ! `verbose` must be a logical (TRUE or FALSE)
    x You Passed: NA

---

    i In index: 1.
    i With name: control_augment.
    Caused by error in `.f()`:
    ! `control_augment` must be a non-null double between 0 and 1.
    x You passed: -1

---

    i In index: 1.
    i With name: control_augment.
    Caused by error in `.f()`:
    ! `control_augment` must be a non-null double between 0 and 1.
    x You passed: 2

---

    i In index: 1.
    i With name: control_augment.
    Caused by error in `.f()`:
    ! `control_augment` must be a non-null double between 0 and 1.
    x You passed: NA

---

    Can't combine `..1$treatment_block` <double> and `..2$treatment_block` <character>.

# Throws proper error when columns do not exist or not declared

    `block2 is not in the data, but was chosen as a block.

---

    i In index: 3.
    Caused by error in `.f()`:
    ! Required column `condition_col` is not found in provided `data`.
    x reason: it is always required
    x Your column: fake_colname

---

    i In index: 1.
    Caused by error in `.f()`:
    ! Required column `id_col` is not found in provided `data`.
    x reason: it is always required
    x Your column: fake_colname

---

    i In index: 2.
    Caused by error in `.f()`:
    ! Required column `success_col` is not found in provided `data`.
    x reason: it is always required
    x Your column: fake_colname

---

    i In index: 6.
    Caused by error in `.f()`:
    ! Required column `success_date_col` is not found in provided `data`.
    x reason: perfect_assignment is FALSE
    x Your column: fake_colname

---

    i In index: 7.
    Caused by error in `.f()`:
    ! Required column `assignment_date_col` is not found in provided `data`.
    x reason: perfect_assignment is FALSE
    x Your column: fake_colname

---

    i In index: 4.
    Caused by error in `.f()`:
    ! Required column `date_col` is not found in provided `data`.
    x reason: assignment_method is 'date'
    x Your column: fake_colname

---

    i In index: 5.
    Caused by error in `.f()`:
    ! Required column `month_col` is not found in provided `data`.
    x reason: time_unit is 'month'
    x Your column: fake_colname

---

    block_cols must be provided when blocking = TRUE.

---

    i In index: 3.
    Caused by error in `.f()`:
    ! Required column `condition_col` is not declared in `data_cols`.
    x reason: it is always required

---

    i In index: 1.
    Caused by error in `.f()`:
    ! Required column `id_col` is not declared in `data_cols`.
    x reason: it is always required

---

    i In index: 2.
    Caused by error in `.f()`:
    ! Required column `success_col` is not declared in `data_cols`.
    x reason: it is always required

---

    i In index: 6.
    Caused by error in `.f()`:
    ! Required column `success_date_col` is not declared in `data_cols`.
    x reason: perfect_assignment is FALSE

---

    i In index: 7.
    Caused by error in `.f()`:
    ! Required column `assignment_date_col` is not declared in `data_cols`.
    x reason: perfect_assignment is FALSE

---

    i In index: 4.
    Caused by error in `.f()`:
    ! Required column `date_col` is not declared in `data_cols`.
    x reason: assignment_method is 'date'

---

    i In index: 5.
    Caused by error in `.f()`:
    ! Required column `month_col` is not declared in `data_cols`.
    x reason: time_unit is 'month'

# Throws Proper Error When Columns are Wrong Data Type

    i In index: 1.
    Caused by error in `.f()`:
    ! Required column `id_col` is the wrong data type.
    x Your type: complex
    i Permissible types: numeric, logical, integer, character, factor, Date, POSIXt

---

    i In index: 2.
    Caused by error in `.f()`:
    ! Required column `success_col` is the wrong data type.
    x Your type: character
    i Permissible types: numeric, logical, integer

---

    i In index: 6.
    Caused by error in `.f()`:
    ! Required column `success_date_col` is the wrong data type.
    x Your type: character
    i Permissible types: Date, POSIXt

---

    i In index: 4.
    Caused by error in `.f()`:
    ! Required column `date_col` is the wrong data type.
    x Your type: character
    i Permissible types: Date, POSIXt

---

    i In index: 7.
    Caused by error in `.f()`:
    ! Required column `assignment_date_col` is the wrong data type.
    x Your type: character
    i Permissible types: Date, POSIXt

---

    i In index: 3.
    Caused by error in `.f()`:
    ! Required column `condition_col` is the wrong data type.
    x Your type: POSIXct, POSIXt
    i Permissible types: numeric, logical, integer, character, factor

---

    i In index: 5.
    Caused by error in `.f()`:
    ! Required column `month_col` is the wrong data type.
    x Your type: complex
    i Permissible types: numeric, integer, character, factor

# Multiple Simulation Specific Error Checks work

    Argument 'times' must be an integer value greater than or equal to 1

---

    Argument 'times' must be an integer value greater than or equal to 1

---

    Argument 'times' must be an integer value greater than or equal to 1

---

    Argument 'seeds' must be an integer vector of length equal to `times`. Please provide a valid vector.
    x You passed a double vector of length 5, while times is 5.
    i Reccomended to use `sample.int()` to create proper vector

---

    Argument 'seeds' must be an integer vector of length equal to `times`. Please provide a valid vector.
    x You passed a character vector of length 5, while times is 5.
    i Reccomended to use `sample.int()` to create proper vector

---

    Argument 'seeds' must be an integer vector of length equal to `times`. Please provide a valid vector.
    x You passed a integer vector of length 4, while times is 5.
    i Reccomended to use `sample.int()` to create proper vector

---

    Argument 'seeds' must be an integer vector of length equal to `times`. Please provide a valid vector.
    x You passed a double vector of length 5, while times is 5.
    i Reccomended to use `sample.int()` to create proper vector

---

    Argument 'keep_data' must logical. Please enter `TRUE` or `FALSE`

---

    Argument 'keep_data' must logical. Please enter `TRUE` or `FALSE`

---

    Argument 'keep_data' must logical. Please enter `TRUE` or `FALSE`


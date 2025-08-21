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

    i In index: 2.
    i With name: blocking.
    Caused by error in `.f()`:
    ! `blocking` must be a logical (TRUE or FALSE)
    x You Passed: 654

---

    i In index: 2.
    i With name: blocking.
    Caused by error in `.f()`:
    ! `blocking` must be a logical (TRUE or FALSE)
    x You Passed: text

---

    i In index: 2.
    i With name: blocking.
    Caused by error in `.f()`:
    ! `blocking` must be a logical (TRUE or FALSE)
    x You Passed: NA

---

    Invalid Time Unit
    x you passed: weeks
    i valid units are `day`, `month`, `week`

---

    Invalid Time Unit
    x you passed: 5
    i valid units are `day`, `month`, `week`

---

    i In index: 4.
    i With name: perfect_assignment.
    Caused by error in `.f()`:
    ! `perfect_assignment` must be a logical (TRUE or FALSE)
    x You Passed: 45

---

    i In index: 4.
    i With name: perfect_assignment.
    Caused by error in `.f()`:
    ! `perfect_assignment` must be a logical (TRUE or FALSE)
    x You Passed: text

---

    i In index: 4.
    i With name: perfect_assignment.
    Caused by error in `.f()`:
    ! `perfect_assignment` must be a logical (TRUE or FALSE)
    x You Passed: NA

---

    i In index: 3.
    i With name: whole_experiment.
    Caused by error in `.f()`:
    ! `whole_experiment` must be a logical (TRUE or FALSE)
    x You Passed: 546

---

    i In index: 3.
    i With name: whole_experiment.
    Caused by error in `.f()`:
    ! `whole_experiment` must be a logical (TRUE or FALSE)
    x You Passed: text

---

    i In index: 3.
    i With name: whole_experiment.
    Caused by error in `.f()`:
    ! `whole_experiment` must be a logical (TRUE or FALSE)
    x You Passed: NA

---

    `prior_periods` must be a positive integer
    x You passed: -5

---

    `prior_periods` must be a positive integer or one of: 'all'
    x You passed: text

---

    `prior_periods` must be a positive integer
    x You passed: NA

---

    `period_length` cannot be larger than data size
    x You data has 10 rows, and your batch size is 50 rows

---

    `period_length` must be a positive integer.
    x You passed: -1

---

    `period_length` must be a positive integer.
    x You passed: text

---

    `period_length` must be a positive integer.
    x You passed: NA

---

    `ndraws` must be a positive integer
    x You passed: -234432

---

    `ndraws` must be a positive integer
    x You passed: NA

---

    `ndraws` must be a positive integer or one of: ''
    x You passed: text

---

    i In index: 2.
    i With name: random_assign_prop.
    Caused by error in `.f()`:
    ! `random_assign_prop` must be a non-null double between 0 and 1.
    x You passed: 23

---

    i In index: 2.
    i With name: random_assign_prop.
    Caused by error in `.f()`:
    ! `random_assign_prop` must be a non-null double between 0 and 1.
    x You passed: -0.4

---

    i In index: 2.
    i With name: random_assign_prop.
    Caused by error in `.f()`:
    ! `random_assign_prop` must be a non-null double between 0 and 1.
    x You passed: NA

---

    i In index: 2.
    i With name: random_assign_prop.
    Caused by error in `.f()`:
    ! `random_assign_prop` must be a non-null double between 0 and 1.
    x You passed: text

---

    `control_condition` is not present in the conditions column
    x Potential Conditions: 1, 2, 3
    x You Passed: 7

---

    `control_condition` is not present in the conditions column
    x Potential Conditions: 1, 2, 3
    x You Passed: NA

---

    `control_condition` must have a length of 1
    x You passed a vector of length: 2

# Throws proper error when columns do not exist or not declared

    Can't extract column with `data_cols$month_col$name`.
    x Subscript `data_cols$month_col$name` must be size 1, not 0.

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


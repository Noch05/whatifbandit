# Throws Proper Error when ID's are not unique

    id is not a unique identifier, a unique id for each observation is required

# Throws Proper Error when arguments are invalid

    'algorithm' must be 'Thompson' or 'UCB1'.
    x You passed: not

---

    'algorithm' must be 'Thompson' or 'UCB1'.
    x You passed: 76

---

    Invalid `assignment_method`
    x you passed: not
    i Valid methods are `Individual`, `Batch`, `Date`

---

    Invalid `assignment_method`
    x you passed: 45
    i Valid methods are `Individual`, `Batch`, `Date`

---

    i In index: 1.
    i With name: verbose.
    Caused by error in `.f()`:
    ! `verbose` must be logical (TRUE or FALSE).

---

    i In index: 1.
    i With name: verbose.
    Caused by error in `.f()`:
    ! `verbose` must be logical (TRUE or FALSE).

---

    `control_augment` must be a non-null double between 0 and 1
    x You passed: -1

---

    `control_augment` must be a non-null double between 0 and 1
    x You passed: 2

---

    `control_augment` must be a non-null double between 0 and 1
    x You passed: NA

---

    i In index: 2.
    i With name: blocking.
    Caused by error in `.f()`:
    ! `blocking` must be logical (TRUE or FALSE).

---

    i In index: 2.
    i With name: blocking.
    Caused by error in `.f()`:
    ! `blocking` must be logical (TRUE or FALSE).

---

    Invalid Time Unit
    x you passed: Weeks
    i valid units are `Day`, `Month`, `Week`

---

    Invalid Time Unit
    x you passed: 5
    i valid units are `Day`, `Month`, `Week`

---

    i In index: 4.
    i With name: perfect_assignment.
    Caused by error in `.f()`:
    ! `perfect_assignment` must be logical (TRUE or FALSE).

---

    i In index: 4.
    i With name: perfect_assignment.
    Caused by error in `.f()`:
    ! `perfect_assignment` must be logical (TRUE or FALSE).

---

    i In index: 3.
    i With name: whole_experiment.
    Caused by error in `.f()`:
    ! `whole_experiment` must be logical (TRUE or FALSE).

---

    i In index: 3.
    i With name: whole_experiment.
    Caused by error in `.f()`:
    ! `whole_experiment` must be logical (TRUE or FALSE).

---

    `prior_periods` must be a positive integer or 'All'.
    x You passed: -5 

---

    `prior_periods` must be a positive integer or 'All'.
    x You passed: text

---

    `period_length` cannot be larger than data size
    x You data has 10, and your batch size is 50

---

    `period_length` must be a positive integer.
    x You passed: -1

---

    `period_length` must be a positive integer.

---

    Conditions passed must be same length as number of unique conditions
    x You passed a vector of length 4
    x Your data has 3 unique treatments

# Columns that do not exist in data are found

    i In index: 3.
    Caused by error in `.f()`:
    ! Required column `condition_col` is not found in provided `data`.
    x reason: it is always required

---

    i In index: 1.
    Caused by error in `.f()`:
    ! Required column `id_col` is not found in provided `data`.
    x reason: it is always required

---

    i In index: 2.
    Caused by error in `.f()`:
    ! Required column `success_col` is not found in provided `data`.
    x reason: it is always required


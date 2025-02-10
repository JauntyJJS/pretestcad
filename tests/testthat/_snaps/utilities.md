# check_if_numeric gives error for character inputs

    Code
      check_if_numeric("5")
    Condition
      Error:
      ! Provided input `"5"`, must be <numeric>, `NA` or `NaN`. It is currently "5" of type <character>

# check_if_numeric gives error for NULL inputs

    Code
      check_if_numeric(NULL)
    Condition
      Error:
      ! Provided input `NULL`, must be <numeric>, `NA` or `NaN`. It is currently of type <NULL>

# check_if_numeric gives error for NA inputs

    Code
      check_if_numeric(NA, allow_na = FALSE)
    Condition
      Error:
      ! Provided input `NA`, must be <numeric>. It is currently NA of type <logical>

# check_if_positive gives error for invalid inputs

    Code
      check_if_positive(-5)
    Condition
      Error:
      ! `-5` must be positive, not -5

---

    Code
      check_if_positive(0)
    Condition
      Error:
      ! `0` must be positive, not 0

# check_if_non_negative gives no error for 0

    Code
      check_if_non_negative(-5)
    Condition
      Error:
      ! `-5` must be non-negative, not -5

# arg_match0_allow_na gives error for invalid inputs

    Code
      arg_match0_allow_na("M", values = c("female", "male"))
    Condition
      Error:
      ! `"M"` must be one of "female" or "male", not "M".

# arg_match0_integer gives error for invalid inputs

    Code
      arg_match0_integer(6, values = c(0:5))
    Condition
      Error:
      ! Provided input `6`, must be 0, 1, 2, 3, 4, 5, NA or NaN. It is currently 6.

# arg_match0_integer gives error for NULL inputs

    Code
      arg_match0_integer(NULL, values = c(0:5))
    Condition
      Error:
      ! Provided input `NULL`, must be 0, 1, 2, 3, 4, 5, NA or NaN. It is currently of type <NULL>

# arg_match0_integer gives error for NA inputs

    Code
      arg_match0_integer(NA, values = c(0:5), allow_na = FALSE)
    Condition
      Error:
      ! Provided input `NA`, must be must be 0, 1, 2, 3, 4 or 5. It is currently NA.

# arg_match0_true_or_false gives error for invalid inputs

    Code
      arg_match0_true_or_false(0)
    Condition
      Error:
      ! Provided input `0`, must be TRUE, FALSE, NA or NaN. It is currently 0.

---

    Code
      arg_match0_true_or_false(1)
    Condition
      Error:
      ! Provided input `1`, must be TRUE, FALSE, NA or NaN. It is currently 1.

# arg_match0_true_or_false gives error for NULL inputs

    Code
      arg_match0_true_or_false(NULL)
    Condition
      Error:
      ! Provided input `NULL`, must be TRUE, FALSE, NA or NaN. It is currently of type <NULL>

# arg_match0_true_or_false gives error for NA inputs

    Code
      arg_match0_true_or_false(NA, allow_na = FALSE)
    Condition
      Error:
      ! Provided input `NA`, must be must be TRUE or FALSE. It is currently NA.


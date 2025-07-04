# check_if_numeric gives error for character inputs

    Code
      check_if_numeric(input)
    Condition
      Error:
      ! Provided input `input`, must be <numeric>, `NA` or `NaN`. It is currently "5" of type <character>

# check_if_numeric gives error for NULL inputs

    Code
      check_if_numeric(input)
    Condition
      Error:
      ! Provided input `input`, must be <numeric>, `NA` or `NaN`. It is currently of type <NULL>

# check_if_numeric gives error for NA inputs

    Code
      check_if_numeric(input, allow_na = FALSE)
    Condition
      Error:
      ! Provided input `input`, must be <numeric>. It is currently NA of type <logical>

# check_if_positive gives error for invalid inputs

    Code
      check_if_positive(input)
    Condition
      Error:
      ! `input` must be positive, not -5

---

    Code
      check_if_positive(input)
    Condition
      Error:
      ! `input` must be positive, not 0

# check_if_non_negative gives error for -5

    Code
      check_if_non_negative(input)
    Condition
      Error:
      ! `input` must be non-negative, not -5

# check_if_integer gives no error for 5.5

    Code
      check_if_integer(input)
    Condition
      Error:
      ! `input` must be an integer, not 5.5. Consider rounding the value to the nearest integer using janitor::round_half_up (<https://sfirke.github.io/janitor/reference/round_half_up.html>) and convert the value to type <integer> using base::as.integer (<https://stat.ethz.ch/R-manual/R-devel/library/base/html/integer.html>) before using the function.

# arg_match0_allow_na gives error for invalid inputs

    Code
      arg_match0_allow_na(input, values = c("female", "male"))
    Condition
      Error:
      ! `input` must be one of "female" or "male", not "emale".
      i Did you mean "female"?

# arg_match0_integer gives error for invalid inputs

    Code
      arg_match0_integer(input, values = c(0:5))
    Condition
      Error:
      ! Provided input `input`, must be 0, 1, 2, 3, 4, 5, NA or NaN. It is currently 6.

# arg_match0_integer gives error for NULL inputs with allow_na set to TRUE

    Code
      arg_match0_integer(input, values = c(0:5))
    Condition
      Error:
      ! Provided input `input`, must be 0, 1, 2, 3, 4, 5, NA or NaN. It is currently of type <NULL>

---

    Code
      arg_match0_integer(input, values = c(0:5), allow_na = FALSE)
    Condition
      Error:
      ! Provided input `input`, must be 0, 1, 2, 3, 4 or 5. It is currently of type <NULL>

# arg_match0_integer gives error for NA inputs

    Code
      arg_match0_integer(input, values = c(0:5), allow_na = FALSE)
    Condition
      Error:
      ! Provided input `input`, must be must be 0, 1, 2, 3, 4 or 5. It is currently NA.

# arg_match0_true_or_false gives error for invalid inputs

    Code
      arg_match0_true_or_false(input)
    Condition
      Error:
      ! Provided input `input`, must be TRUE, FALSE, NA or NaN. It is currently 0.

---

    Code
      arg_match0_true_or_false(input)
    Condition
      Error:
      ! Provided input `input`, must be TRUE, FALSE, NA or NaN. It is currently 1.

# arg_match0_true_or_false gives error for NULL inputs with allow_na set to TRUE

    Code
      arg_match0_true_or_false(input)
    Condition
      Error:
      ! Provided input `input`, must be TRUE, FALSE, NA or NaN. It is currently of type <NULL>

# arg_match0_true_or_false gives error for NULL inputs with allow_na set to FALSE

    Code
      arg_match0_true_or_false(input, allow_na = FALSE)
    Condition
      Error:
      ! Provided input `input`, must be TRUE or FALSE. It is currently of type <NULL>

# arg_match0_true_or_false gives error for NA inputs

    Code
      arg_match0_true_or_false(input, allow_na = FALSE)
    Condition
      Error:
      ! Provided input `input`, must be must be TRUE or FALSE. It is currently NA.


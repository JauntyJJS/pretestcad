# calculate_aha_2012_tbl_9_ptp gives error of invalid missing input of sex

    Code
      calculate_aha_2012_tbl_9_ptp(age = 55, sex = NA, chest_pain_type = "typical",
        label_sex_male = c("male"), label_sex_female = c("female"),
        label_sex_unknown = c("NIL"))
    Condition
      Error in `calculate_aha_2012_tbl_9_ptp()`:
      ! `sex` must be one of "male", "female", or "NIL" not NA.

# calculate_aha_2012_tbl_9_ptp gives error of invalid male input

    Code
      calculate_aha_2012_tbl_9_ptp(age = 55, sex = "Male", chest_pain_type = "typical",
        label_sex_male = c("male"), label_sex_female = c("female"),
        label_sex_unknown = c(NA, NaN))
    Condition
      Error in `calculate_aha_2012_tbl_9_ptp()`:
      ! `sex` must be one of "male", "female", "NA", or "NaN", not "Male".
      i Did you mean "male"?

# calculate_aha_2012_tbl_9_ptp gives error of invalid missing input of chest pain type

    Code
      calculate_aha_2012_tbl_9_ptp(age = 55, sex = "male", chest_pain_type = NA,
        label_cpt_nonanginal = c("nonanginal"), label_cpt_atypical = c("atypical"),
        label_cpt_typical = c("typical"), label_cpt_unknown = c("NIL"))
    Condition
      Error in `calculate_aha_2012_tbl_9_ptp()`:
      ! `chest_pain_type` must be one of "nonanginal", "atypical", "typical", or "NIL" not NA.

# calculate_aha_2012_tbl_9_ptp gives error of invalid typical chest pain input

    Code
      calculate_aha_2012_tbl_9_ptp(age = 55, sex = "male", chest_pain_type = "Typical",
        label_cpt_nonanginal = c("nonanginal"), label_cpt_atypical = c("atypical"),
        label_cpt_typical = c("typical"), label_cpt_unknown = c(NA, NaN))
    Condition
      Error in `calculate_aha_2012_tbl_9_ptp()`:
      ! `chest_pain_type` must be one of "nonanginal", "atypical", "typical", "NA", or "NaN", not "Typical".
      i Did you mean "typical"?


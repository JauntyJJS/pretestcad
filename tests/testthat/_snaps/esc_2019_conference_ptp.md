# calculate_esc_2019_ptp gives error of invalid missing input of sex

    Code
      calculate_esc_2019_ptp(age = 55, sex = NA, have_dyspnoea = "no",
        chest_pain_type = "typical", label_sex_male = c("male"), label_sex_female = c(
          "female"), label_sex_unknown = c("NIL"))
    Condition
      Error in `calculate_esc_2019_ptp()`:
      ! `sex` must be one of "male", "female", or "NIL" not NA.

# calculate_esc_2019_ptp gives error of invalid male input

    Code
      calculate_esc_2019_ptp(age = 55, sex = "Male", have_dyspnoea = "no",
        chest_pain_type = "typical", label_sex_male = c("male"), label_sex_female = c(
          "female"), label_sex_unknown = c(NA, NaN))
    Condition
      Error in `calculate_esc_2019_ptp()`:
      ! `sex` must be one of "male", "female", "NA", or "NaN", not "Male".
      i Did you mean "male"?

# calculate_esc_2019_ptp gives error of invalid missing input of dyspnoea

    Code
      calculate_esc_2019_ptp(age = 55, sex = "male", have_dyspnoea = NA,
        chest_pain_type = "no chest pain", label_have_dyspnoea_no = c("no"),
        label_have_dyspnoea_yes = c("yes"), label_have_dyspnoea_unknown = c("NIL"))
    Condition
      Error in `calculate_esc_2019_ptp()`:
      ! `have_dyspnoea` must be one of "no", "yes", or "NIL" not NA.

# calculate_esc_2019_ptp gives error of invalid no dyspnoea input

    Code
      calculate_esc_2019_ptp(age = 55, sex = "male", have_dyspnoea = "No",
        chest_pain_type = "no chest pain", label_have_dyspnoea_no = c("no"),
        label_have_dyspnoea_yes = c("yes"), label_have_dyspnoea_unknown = c(NA, NaN))
    Condition
      Error in `calculate_esc_2019_ptp()`:
      ! `have_dyspnoea` must be one of "no", "yes", "NA", or "NaN", not "No".
      i Did you mean "no"?

# calculate_esc_2019_ptp gives error of invalid missing input of chest pain type

    Code
      calculate_esc_2019_ptp(age = 55, sex = "male", have_dyspnoea = "no",
        chest_pain_type = NA, label_cpt_no_chest_pain = c("no chest pain"),
        label_cpt_nonanginal = c("nonanginal"), label_cpt_atypical = c("atypical"),
        label_cpt_typical = c("typical"), label_cpt_unknown = c("NIL"))
    Condition
      Error in `calculate_esc_2019_ptp()`:
      ! `chest_pain_type` must be one of "no chest pain", "nonanginal", "atypical", "typical", or "NIL" not NA.

# calculate_esc_2019_ptp gives error of invalid typical chest pain input

    Code
      calculate_esc_2019_ptp(age = 55, sex = "male", have_dyspnoea = "no",
        chest_pain_type = "Typical", label_cpt_no_chest_pain = c("no chest pain"),
        label_cpt_nonanginal = c("nonanginal"), label_cpt_atypical = c("atypical"),
        label_cpt_typical = c("typical"), label_cpt_unknown = c(NA, NaN))
    Condition
      Error in `calculate_esc_2019_ptp()`:
      ! `chest_pain_type` must be one of "no chest pain", "nonanginal", "atypical", "typical", "NA", or "NaN", not "Typical".
      i Did you mean "typical"?


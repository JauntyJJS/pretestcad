# calculate_esc_2019_ptp gives error of invalid text input of age

    Code
      calculate_esc_2024_fig_4_ptp(age = "Something", sex = "male", chest_pain_type = "typical",
        have_dyspnoea = "no", have_family_history = "no", have_smoking_history = "no",
        have_dyslipidemia = "no", have_hypertension = "no", have_diabetes = "no",
        allow_na_symptom_score = TRUE, max_na_num_of_rf = 0, output = "numeric")
    Condition
      Error in `calculate_esc_2024_fig_4_ptp()`:
      ! Provided input `age`, must be <numeric>, `NA` or `NaN`. It is currently "Something" of type <character>

# calculate_esc_2019_ptp gives error of invalid output option

    Code
      calculate_esc_2024_fig_4_ptp(age = 55, sex = "male", chest_pain_type = "typical",
        have_dyspnoea = "no", have_family_history = "no", have_smoking_history = "no",
        have_dyslipidemia = "no", have_hypertension = "no", have_diabetes = "no",
        allow_na_symptom_score = TRUE, max_na_num_of_rf = 0, output = "Something Else",
        )
    Condition
      Error in `calculate_esc_2024_fig_4_ptp()`:
      ! `output` must be one of "grouping", "numeric", or "percentage", not "Something Else".

# calculate_esc_2019_ptp gives error of invalid non-positive input of age

    Code
      calculate_esc_2024_fig_4_ptp(age = 0, sex = "male", chest_pain_type = "typical",
        have_dyspnoea = "no", have_family_history = "no", have_smoking_history = "no",
        have_dyslipidemia = "no", have_hypertension = "no", have_diabetes = "no",
        allow_na_symptom_score = TRUE, max_na_num_of_rf = 0, output = "numeric")
    Condition
      Error in `calculate_esc_2024_fig_4_ptp()`:
      ! `age` must be positive, not 0

# calculate_esc_2019_ptp gives error of invalid non-integer input of age

    Code
      calculate_esc_2024_fig_4_ptp(age = 77.7, sex = "male", chest_pain_type = "typical",
        have_dyspnoea = "no", have_family_history = "no", have_smoking_history = "no",
        have_dyslipidemia = "no", have_hypertension = "no", have_diabetes = "no",
        allow_na_symptom_score = TRUE, max_na_num_of_rf = 0, output = "numeric")
    Condition
      Error in `calculate_esc_2024_fig_4_ptp()`:
      ! `age` must be an integer, not 77.7. Consider rounding the value to the nearest integer using janitor::round_half_up (<https://sfirke.github.io/janitor/reference/round_half_up.html>) and convert the value to type <integer> using base::as.integer (<https://stat.ethz.ch/R-manual/R-devel/library/base/html/integer.html>) before using the function.

# calculate_esc_2019_ptp gives error of invalid input of sex labels

    Code
      calculate_esc_2024_fig_4_ptp(age = 55, sex = "male", chest_pain_type = "typical",
        have_dyspnoea = "no", have_family_history = "no", have_smoking_history = "no",
        have_dyslipidemia = "no", have_hypertension = "no", have_diabetes = "no",
        allow_na_symptom_score = TRUE, max_na_num_of_rf = 0, output = "numeric",
        label_sex_male = c("M"), label_sex_female = c("M"), label_sex_unknown = c("M"))
    Condition
      Error in `calculate_esc_2024_fig_4_ptp()`:
      ! `label_sex_male`, `label_sex_female` and `label_sex_unknown` must be mutually exclusive.
      Common values found in `label_sex_male` and `label_sex_female`: "M".
      Common values found in `label_sex_male` and `label_sex_unknown`: "M".
      Common values found in `label_sex_female` and `label_sex_unknown`: "M".
      Please ensure `label_sex_male`, `label_sex_female` and `label_sex_unknown` do not hold common values.

# calculate_esc_2019_ptp gives error of invalid input of sex

    Code
      calculate_esc_2024_fig_4_ptp(age = 55, sex = "male", chest_pain_type = "typical",
        have_dyspnoea = "no", have_family_history = "no", have_smoking_history = "no",
        have_dyslipidemia = "no", have_hypertension = "no", have_diabetes = "no",
        allow_na_symptom_score = TRUE, max_na_num_of_rf = 0, output = "numeric",
        label_sex_male = c("M"), label_sex_female = c("F"))
    Condition
      Error in `calculate_esc_2024_fig_4_ptp()`:
      ! `sex` must be one of "M", "F", "NA", or "NaN", not "male".

# calculate_esc_2019_ptp gives error of invalid input of chest_pain_type labels

    Code
      calculate_esc_2024_fig_4_ptp(age = 55, sex = "male", chest_pain_type = "typical",
        have_dyspnoea = "no", have_family_history = "no", have_smoking_history = "no",
        have_dyslipidemia = "no", have_hypertension = "no", have_diabetes = "no",
        allow_na_symptom_score = TRUE, max_na_num_of_rf = 0, output = "numeric",
        label_cpt_no_chest_pain = c("nonanginal"), label_cpt_nonanginal = c(
          "nonanginal"), label_cpt_atypical = c("typical"), label_cpt_typical = c(
          "typical"), label_cpt_unknown = c("nonanginal", "typical"), )
    Condition
      Error in `calculate_esc_2024_fig_4_ptp()`:
      ! `label_cpt_no_chest_pain`, `label_cpt_nonanginal`, `label_cpt_atypical`, `label_cpt_typical` and `label_cpt_unknown` must be mutually exclusive.
      Common values found in `label_cpt_no_chest_pain` and `label_cpt_nonanginal`: "nonanginal".
      Common values found in `label_cpt_atypical` and `label_cpt_typical`: "typical".
      Common values found in `label_cpt_no_chest_pain` and `label_cpt_unknown`: "nonanginal".
      Common values found in `label_cpt_nonanginal` and `label_cpt_unknown`: "nonanginal".
      Common values found in `label_cpt_atypical` and `label_cpt_unknown`: "typical".
      Common values found in `label_cpt_typical` and `label_cpt_unknown`: "typical".
      Please ensure `label_cpt_no_chest_pain`, `label_cpt_nonanginal`, `label_cpt_atypical`, `label_cpt_typical` and `label_cpt_unknown` do not hold common values.

# calculate_esc_2019_ptp gives error of invalid input of chest_pain_type

    Code
      calculate_esc_2024_fig_4_ptp(age = 55, sex = "male", chest_pain_type = "Typical",
        have_dyspnoea = "no", have_family_history = "no", have_smoking_history = "no",
        have_dyslipidemia = "no", have_hypertension = "no", have_diabetes = "no",
        allow_na_symptom_score = TRUE, max_na_num_of_rf = 0, output = "numeric",
        label_cpt_no_chest_pain = c("no chest pain"), label_cpt_nonanginal = c(
          "nonanginal"), label_cpt_atypical = c("atypical"), label_cpt_typical = c(
          "typical"), label_cpt_unknown = c(NA, NaN), )
    Condition
      Error in `calculate_esc_2024_fig_4_ptp()`:
      ! `chest_pain_type` must be one of "no chest pain", "nonanginal", "atypical", "typical", "NA", or "NaN", not "Typical".
      i Did you mean "typical"?


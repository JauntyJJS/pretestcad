# harmonise_two_labels gives error of invalid missing input of have_dyspnoea_test

    Code
      harmonise_two_labels(arg = have_dyspnoea_test, label_one = label_have_dyspnoea_no,
        label_two = label_have_dyspnoea_yes, label_unknown = label_have_dyspnoea_unknown,
        harmonise_label_one = "no", harmonise_label_two = "yes",
        harmonise_label_unknown = NA)
    Condition
      Error:
      ! `have_dyspnoea_test` must be one of "no", "yes", or "NIL" not NA.

# harmonise_two_labels gives error of invalid non-missing input with partial match

    Code
      harmonise_two_labels(arg = have_dyspnoea_test, label_one = label_have_dyspnoea_no,
        label_two = label_have_dyspnoea_yes, label_unknown = label_have_dyspnoea_unknown,
        harmonise_label_one = "no", harmonise_label_two = "yes",
        harmonise_label_unknown = NA)
    Condition
      Error:
      ! `have_dyspnoea_test` must be one of "no", "yes", "NA", or "NaN", not "No".
      i Did you mean "no"?

# harmonise_two_labels gives error of invalid non-missing input with no partial match

    Code
      harmonise_two_labels(arg = have_dyspnoea_test, label_one = label_have_dyspnoea_no,
        label_two = label_have_dyspnoea_yes, label_unknown = label_have_dyspnoea_unknown,
        harmonise_label_one = "no", harmonise_label_two = "yes",
        harmonise_label_unknown = NA)
    Condition
      Error:
      ! `have_dyspnoea_test` must be one of "no", "yes", "NA", or "NaN", not "Something".

# harmonise_three_labels gives error of invalid missing input of chest pain type

    Code
      harmonise_three_labels(arg = chest_pain_type_test, label_one = label_cpt_nonanginal,
        label_two = label_cpt_atypical, label_three = label_cpt_typical,
        label_unknown = label_cpt_unknown, harmonise_label_one = "nonanginal",
        harmonise_label_two = "atypical", harmonise_label_three = "typical",
        harmonise_label_unknown = NA)
    Condition
      Error:
      ! `chest_pain_type_test` must be one of "nonanginal", "atypical", "typical", or "NIL" not NA.

# harmonise_three_labels gives error of invalid typical chest pain input with partial match

    Code
      harmonise_three_labels(arg = chest_pain_type_test, label_one = label_cpt_nonanginal,
        label_two = label_cpt_atypical, label_three = label_cpt_typical,
        label_unknown = label_cpt_unknown, harmonise_label_one = "nonanginal",
        harmonise_label_two = "atypical", harmonise_label_three = "typical",
        harmonise_label_unknown = NA)
    Condition
      Error:
      ! `chest_pain_type_test` must be one of "nonanginal", "atypical", "typical", "NA", or "NaN", not "Typical".
      i Did you mean "typical"?

# harmonise_three_labels gives error of invalid typical chest pain input without partial match

    Code
      harmonise_three_labels(arg = chest_pain_type_test, label_one = label_cpt_nonanginal,
        label_two = label_cpt_atypical, label_three = label_cpt_typical,
        label_unknown = label_cpt_unknown, harmonise_label_one = "nonanginal",
        harmonise_label_two = "atypical", harmonise_label_three = "typical",
        harmonise_label_unknown = NA)
    Condition
      Error:
      ! `chest_pain_type_test` must be one of "nonanginal", "atypical", "typical", "NA", or "NaN", not "Something".

# harmonise_four_labels gives error of invalid missing input of chest pain type

    Code
      harmonise_four_labels(arg = chest_pain_type_test, label_one = label_cpt_no_chest_pain,
        label_two = label_cpt_nonanginal, label_three = label_cpt_atypical,
        label_four = label_cpt_typical, label_unknown = label_cpt_unknown,
        harmonise_label_one = "no chest pain", harmonise_label_two = "nonanginal",
        harmonise_label_three = "atypical", harmonise_label_four = "typical",
        harmonise_label_unknown = NA)
    Condition
      Error:
      ! `chest_pain_type_test` must be one of "no chest pain", "normal", "nonanginal", "atypical", "typical", or "NIL" not NA.

# harmonise_four_labels gives error of invalid typical chest pain input with partial match

    Code
      harmonise_four_labels(arg = chest_pain_type_test, label_one = label_cpt_no_chest_pain,
        label_two = label_cpt_nonanginal, label_three = label_cpt_atypical,
        label_four = label_cpt_typical, label_unknown = label_cpt_unknown,
        harmonise_label_one = "no chest pain", harmonise_label_two = "nonanginal",
        harmonise_label_three = "atypical", harmonise_label_four = "typical",
        harmonise_label_unknown = NA)
    Condition
      Error:
      ! `chest_pain_type_test` must be one of "no chest pain", "normal", "nonanginal", "atypical", "typical", "NA", or "NaN", not "Typical".
      i Did you mean "typical"?

# harmonise_four_labels gives error of invalid typical chest pain input without partial match

    Code
      harmonise_four_labels(arg = chest_pain_type_test, label_one = label_cpt_no_chest_pain,
        label_two = label_cpt_nonanginal, label_three = label_cpt_atypical,
        label_four = label_cpt_typical, label_unknown = label_cpt_unknown,
        harmonise_label_one = "no chest pain", harmonise_label_two = "nonanginal",
        harmonise_label_three = "atypical", harmonise_label_four = "typical",
        harmonise_label_unknown = NA)
    Condition
      Error:
      ! `chest_pain_type_test` must be one of "no chest pain", "normal", "nonanginal", "atypical", "typical", "NA", or "NaN", not "Something".


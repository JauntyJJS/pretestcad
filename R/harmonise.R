#' @title Harmonise Two Labels
#' @description Function to map an input from
#' two different list into one of the two standardise labels
#' @inheritParams harmonise_four_labels
#' @return Character representing one of the two standardise labels.
#' @examples
#' label_have_dyspnoea_no <- c("no", "No")
#' label_have_dyspnoea_yes <- c("yes", "Yes")
#' label_have_dyspnoea_unknown <- c(NA, NaN)
#'
#' # Gives harmonise_label_one if there is valid input of have_dyspnoea
#' have_dyspnoea <- "No"
#'
#' harmonise_two_labels(
#'   arg = have_dyspnoea,
#'   label_one = label_have_dyspnoea_no,
#'   label_two = label_have_dyspnoea_yes,
#'   label_unknown = label_have_dyspnoea_unknown,
#'   harmonise_label_one = "no",
#'   harmonise_label_two = "yes",
#'   harmonise_label_unknown = NA
#' )
#'
#' # Gives harmonise_label_two if there is valid input of have_dyspnoea
#' have_dyspnoea <- "Yes"
#'
#' harmonise_two_labels(
#'   arg = have_dyspnoea,
#'   label_one = label_have_dyspnoea_no,
#'   label_two = label_have_dyspnoea_yes,
#'   label_unknown = label_have_dyspnoea_unknown,
#'   harmonise_label_one = "no",
#'   harmonise_label_two = "yes",
#'   harmonise_label_unknown = NA
#' )
#'
#' # Gives harmonise_label_unknown if there is valid missing input of have_dyspnoea
#' have_dyspnoea <- NaN
#'
#' harmonise_two_labels(
#'   arg = have_dyspnoea,
#'   label_one = label_have_dyspnoea_no,
#'   label_two = label_have_dyspnoea_yes,
#'   label_unknown = label_have_dyspnoea_unknown,
#'   harmonise_label_one = "no",
#'   harmonise_label_two = "yes",
#'   harmonise_label_unknown = NA
#' )
#'
#' # Gives harmonise_label_unknown if there is valid missing input of have_dyspnoea
#' have_dyspnoea <- "NIL"
#' label_have_dyspnoea_unknown <- c("NIL")
#'
#' harmonise_two_labels(
#'   arg = have_dyspnoea,
#'   label_one = label_have_dyspnoea_no,
#'   label_two = label_have_dyspnoea_yes,
#'   label_unknown = label_have_dyspnoea_unknown,
#'   harmonise_label_one = "no",
#'   harmonise_label_two = "yes",
#'   harmonise_label_unknown = NA
#' )
#'
#' # Gives error of invalid have_dyspnoea input with partial match
#' have_dyspnoea <- "Not"
#' label_have_dyspnoea_unknown <- c(NA, NaN)
#'
#' try(harmonise_two_labels(
#'    arg = have_dyspnoea,
#'    label_one = label_have_dyspnoea_no,
#'    label_two = label_have_dyspnoea_yes,
#'    label_unknown = label_have_dyspnoea_unknown,
#'    harmonise_label_one = "no",
#'    harmonise_label_two = "yes",
#'    harmonise_label_unknown = NA
#' ))
#'
#' # Gives error of invalid have_dyspnoea input without partial match
#' have_dyspnoea <- "Something"
#' label_have_dyspnoea_unknown <- c(NA, NaN)
#'
#' try(harmonise_two_labels(
#'    arg = have_dyspnoea,
#'    label_one = label_have_dyspnoea_no,
#'    label_two = label_have_dyspnoea_yes,
#'    label_unknown = label_have_dyspnoea_unknown,
#'    harmonise_label_one = "no",
#'    harmonise_label_two = "yes",
#'    harmonise_label_unknown = NA
#' ))
#'
#' # Gives error of invalid missing input of have_dyspnoea
#' have_dyspnoea <- NA
#' label_have_dyspnoea_unknown <- c("NIL")
#'
#' try(harmonise_two_labels(
#'    arg = have_dyspnoea,
#'    label_one = label_have_dyspnoea_no,
#'    label_two = label_have_dyspnoea_yes,
#'    label_unknown = label_have_dyspnoea_unknown,
#'    harmonise_label_one = "no",
#'    harmonise_label_two = "yes",
#'    harmonise_label_unknown = NA
#' ))
#'
#'
#' @rdname harmonise_two_labels
#' @export
harmonise_two_labels <- function(
    arg,
    label_one,
    label_two,
    label_unknown,
    harmonise_label_one = "no",
    harmonise_label_two = "yes",
    harmonise_label_unknown = NA,
    error_call = rlang::caller_env()
)
{
  # Ensure arg is valid and mapped to a unified group
  # (harmonise_label_one, harmonise_label_two, harmonise_label_unknown)
  label_all <- c(label_one, label_two, label_unknown)

  if (any(is.na(label_all))) {
    label_all[is.na(label_all)] <- "NA"
    arg <- arg_match0_allow_na(
      arg = arg,
      values = label_all,
      arg_nm = rlang::caller_arg(arg),
      error_call = error_call)
  } else if (is.null(arg) || is.na(arg)) {
    arg_match0_no_na_error_message(
      arg = arg,
      values = label_all,
      arg_nm = rlang::caller_arg(arg),
      error_call = error_call
    )
  } else {
    arg <- rlang::arg_match0(
      arg = arg,
      values = label_all,
      arg_nm = rlang::caller_arg(arg),
      error_call = error_call
      )
  }

  arg <- dplyr::case_when(
    arg %in% label_one ~ harmonise_label_one,
    arg %in% label_two ~ harmonise_label_two,
    .default = harmonise_label_unknown
  )

  return(arg)

}

#' @title Harmonise Three Labels
#' @description Function to map an input from
#' three different list into one of the three standardise labels
#' @inheritParams harmonise_four_labels
#' @return Character representing one of the three standardise labels.
#' @examples
#' label_cpt_nonanginal <- c("nonanginal", "unspecified")
#' label_cpt_atypical <- c("atypical", "Atypical")
#' label_cpt_typical <- c("typical", "angina")
#' label_cpt_unknown <- c(NA, NaN)
#'
#' # Gives harmonise_label_one if there is valid input of chest_pain_type
#' chest_pain_type <- "unspecified"
#'
#' harmonise_three_labels(
#'    arg = chest_pain_type,
#'    label_one = label_cpt_nonanginal,
#'    label_two = label_cpt_atypical,
#'    label_three = label_cpt_typical,
#'    label_unknown = label_cpt_unknown,
#'    harmonise_label_one = "nonanginal",
#'    harmonise_label_two = "atypical",
#'    harmonise_label_three = "typical",
#'    harmonise_label_unknown = NA
#' )
#'
#' # Gives harmonise_label_two if there is valid input of chest_pain_type
#' chest_pain_type <- "Atypical"
#'
#' harmonise_three_labels(
#'    arg = chest_pain_type,
#'    label_one = label_cpt_nonanginal,
#'    label_two = label_cpt_atypical,
#'    label_three = label_cpt_typical,
#'    label_unknown = label_cpt_unknown,
#'    harmonise_label_one = "nonanginal",
#'    harmonise_label_two = "atypical",
#'    harmonise_label_three = "typical",
#'    harmonise_label_unknown = NA
#' )
#'
#' # Gives harmonise_label_three if there is valid input of chest_pain_type
#' chest_pain_type <- "angina"
#'
#' harmonise_three_labels(
#'    arg = chest_pain_type,
#'    label_one = label_cpt_nonanginal,
#'    label_two = label_cpt_atypical,
#'    label_three = label_cpt_typical,
#'    label_unknown = label_cpt_unknown,
#'    harmonise_label_one = "nonanginal",
#'    harmonise_label_two = "atypical",
#'    harmonise_label_three = "typical",
#'    harmonise_label_unknown = NA
#' )
#'
#' # Gives harmonise_label_unknown if there is valid missing input of chest_pain_type
#' chest_pain_type <- NaN
#'
#' harmonise_three_labels(
#'    arg = chest_pain_type,
#'    label_one = label_cpt_nonanginal,
#'    label_two = label_cpt_atypical,
#'    label_three = label_cpt_typical,
#'    label_unknown = label_cpt_unknown,
#'    harmonise_label_one = "nonanginal",
#'    harmonise_label_two = "atypical",
#'    harmonise_label_three = "typical",
#'    harmonise_label_unknown = NA
#' )
#'
#' # Gives harmonise_label_unknown if there is valid missing input of chest_pain_type
#' chest_pain_type <- "NIL"
#' label_cpt_unknown <- c("NIL")
#'
#' harmonise_three_labels(
#'    arg = chest_pain_type,
#'    label_one = label_cpt_nonanginal,
#'    label_two = label_cpt_atypical,
#'    label_three = label_cpt_typical,
#'    label_unknown = label_cpt_unknown,
#'    harmonise_label_one = "nonanginal",
#'    harmonise_label_two = "atypical",
#'    harmonise_label_three = "typical",
#'    harmonise_label_unknown = NA
#' )
#'
#' # Gives error of invalid typical chest pain input with partial match
#' chest_pain_type <- "Typical"
#' label_cpt_unknown <- c(NA, NaN)
#'
#' try(harmonise_three_labels(
#'    arg = chest_pain_type,
#'    label_one = label_cpt_nonanginal,
#'    label_two = label_cpt_atypical,
#'    label_three = label_cpt_typical,
#'    label_unknown = label_cpt_unknown,
#'    harmonise_label_one = "nonanginal",
#'    harmonise_label_two = "atypical",
#'    harmonise_label_three = "typical",
#'    harmonise_label_unknown = NA
#' ))
#'
#' # Gives error of invalid typical chest pain input without partial match
#' chest_pain_type <- "Something"
#' label_cpt_unknown <- c(NA, NaN)
#'
#' try(harmonise_three_labels(
#'    arg = chest_pain_type,
#'    label_one = label_cpt_nonanginal,
#'    label_two = label_cpt_atypical,
#'    label_three = label_cpt_typical,
#'    label_unknown = label_cpt_unknown,
#'    harmonise_label_one = "nonanginal",
#'    harmonise_label_two = "atypical",
#'    harmonise_label_three = "typical",
#'    harmonise_label_unknown = NA
#' ))
#'
#' # Gives error of invalid missing input of chest_pain_type
#' chest_pain_type <- NA
#' label_cpt_unknown <- c("NIL")
#'
#' try(harmonise_three_labels(
#'    arg = chest_pain_type,
#'    label_one = label_cpt_nonanginal,
#'    label_two = label_cpt_atypical,
#'    label_three = label_cpt_typical,
#'    label_unknown = label_cpt_unknown,
#'    harmonise_label_one = "nonanginal",
#'    harmonise_label_two = "atypical",
#'    harmonise_label_three = "typical",
#'    harmonise_label_unknown = NA
#' ))
#'
#'
#' @rdname harmonise_three_labels
#' @export
harmonise_three_labels <- function(
    arg,
    label_one,
    label_two,
    label_three,
    label_unknown,
    harmonise_label_one = "group_1",
    harmonise_label_two = "group_2",
    harmonise_label_three = "group_3",
    harmonise_label_unknown = NA,
    error_call = rlang::caller_env()
)
{
  # Ensure arg is valid and mapped to a unified group
  # (harmonise_label_one, harmonise_label_two,
  #  harmonise_label_three, harmonise_label_four,
  #  harmonise_label_unknown)
  label_all <- c(label_one, label_two, label_three, label_unknown)

  if (any(is.na(label_all))) {
    label_all[is.na(label_all)] <- "NA"
    arg <- arg_match0_allow_na(
      arg = arg,
      values = label_all,
      arg_nm = rlang::caller_arg(arg),
      error_call = error_call)
  } else if (is.null(arg) || is.na(arg)) {
    arg_match0_no_na_error_message(
      arg = arg,
      values = label_all,
      arg_nm = rlang::caller_arg(arg),
      error_call = error_call
    )
  } else {
    arg <- rlang::arg_match0(
      arg = arg,
      values = label_all,
      arg_nm = rlang::caller_arg(arg),
      error_call = error_call
    )
  }

  arg <- dplyr::case_when(
    arg %in% label_one ~ harmonise_label_one,
    arg %in% label_two ~ harmonise_label_two,
    arg %in% label_three ~ harmonise_label_three,
    .default = harmonise_label_unknown
  )

  return(arg)

}

#' @title Harmonise Four Labels
#' @description Function to map an input from
#' four different list into one of the four standardise labels
#' @inheritParams rlang::args_error_context
#' @param arg Input argument, in characters to be harmonised
#' @param label_one
#' Input character vector representing the ways to identify \code{harmonise_label_one}
#' @param label_two
#' Input character vector representing the ways to identify \code{harmonise_label_two}
#' @param label_three
#' Input character vector representing the ways to identify \code{harmonise_label_three}
#' @param label_four
#' Input character vector representing the ways to identify \code{harmonise_label_four}
#' @param label_unknown
#' Input character vector representing the ways to identify \code{harmonise_label_unknown}
#' @param harmonise_label_one
#' Input character representing the harmonised label for \code{label_one}
#' Default: 'group_1'
#' @param harmonise_label_two
#' Input character representing the harmonised label for \code{label_two}
#' Default: 'group_2'
#' @param harmonise_label_three
#' Input character representing the harmonised label for \code{label_three}
#' Default: 'group_3'
#' @param harmonise_label_four
#' Input character representing the harmonised label for \code{label_four}
#' Default: 'group_4'
#' @param harmonise_label_unknown
#' Input character representing the harmonised label for \code{label_unknown}
#' Default: NA
#' @return Character representing one of the four standardise labels.
#' @examples
#' label_cpt_no_chest_pain <- c("no chest pain", "normal")
#' label_cpt_nonanginal <- c("nonanginal", "unspecified")
#' label_cpt_atypical <- c("atypical", "Atypical")
#' label_cpt_typical <- c("typical", "angina")
#' label_cpt_unknown <- c(NA, NaN)
#'
#' # Gives harmonise_label_one if there is valid input of chest_pain_type
#' chest_pain_type <- "normal"
#'
#' harmonise_four_labels(
#'    arg = chest_pain_type,
#'    label_one = label_cpt_no_chest_pain,
#'    label_two = label_cpt_nonanginal,
#'    label_three = label_cpt_atypical,
#'    label_four = label_cpt_typical,
#'    label_unknown = label_cpt_unknown,
#'    harmonise_label_one = "no chest pain",
#'    harmonise_label_two = "nonanginal",
#'    harmonise_label_three = "atypical",
#'    harmonise_label_four = "typical",
#'    harmonise_label_unknown = NA
#' )
#'
#' # Gives harmonise_label_two if there is valid input of chest_pain_type
#' chest_pain_type <- "unspecified"
#'
#' harmonise_four_labels(
#'    arg = chest_pain_type,
#'    label_one = label_cpt_no_chest_pain,
#'    label_two = label_cpt_nonanginal,
#'    label_three = label_cpt_atypical,
#'    label_four = label_cpt_typical,
#'    label_unknown = label_cpt_unknown,
#'    harmonise_label_one = "no chest pain",
#'    harmonise_label_two = "nonanginal",
#'    harmonise_label_three = "atypical",
#'    harmonise_label_four = "typical",
#'    harmonise_label_unknown = NA
#' )
#'
#' # Gives harmonise_label_three if there is valid input of chest_pain_type
#' chest_pain_type <- "Atypical"
#'
#' harmonise_four_labels(
#'    arg = chest_pain_type,
#'    label_one = label_cpt_no_chest_pain,
#'    label_two = label_cpt_nonanginal,
#'    label_three = label_cpt_atypical,
#'    label_four = label_cpt_typical,
#'    label_unknown = label_cpt_unknown,
#'    harmonise_label_one = "no chest pain",
#'    harmonise_label_two = "nonanginal",
#'    harmonise_label_three = "atypical",
#'    harmonise_label_four = "typical",
#'    harmonise_label_unknown = NA
#' )
#'
#' # Gives harmonise_label_four if there is valid input of chest_pain_type
#' chest_pain_type <- "angina"
#'
#' harmonise_four_labels(
#'    arg = chest_pain_type,
#'    label_one = label_cpt_no_chest_pain,
#'    label_two = label_cpt_nonanginal,
#'    label_three = label_cpt_atypical,
#'    label_four = label_cpt_typical,
#'    label_unknown = label_cpt_unknown,
#'    harmonise_label_one = "no chest pain",
#'    harmonise_label_two = "nonanginal",
#'    harmonise_label_three = "atypical",
#'    harmonise_label_four = "typical",
#'    harmonise_label_unknown = NA
#' )
#'
#' # Gives harmonise_label_unknown if there is valid missing input of chest_pain_type
#' chest_pain_type <- NaN
#'
#' harmonise_four_labels(
#'    arg = chest_pain_type,
#'    label_one = label_cpt_no_chest_pain,
#'    label_two = label_cpt_nonanginal,
#'    label_three = label_cpt_atypical,
#'    label_four = label_cpt_typical,
#'    label_unknown = label_cpt_unknown,
#'    harmonise_label_one = "no chest pain",
#'    harmonise_label_two = "nonanginal",
#'    harmonise_label_three = "atypical",
#'    harmonise_label_four = "typical",
#'    harmonise_label_unknown = NA
#' )
#'
#' # Gives harmonise_label_unknown if there is valid missing input of chest_pain_type
#' chest_pain_type <- "NIL"
#' label_cpt_unknown <- c("NIL")
#'
#' harmonise_four_labels(
#'    arg = chest_pain_type,
#'    label_one = label_cpt_no_chest_pain,
#'    label_two = label_cpt_nonanginal,
#'    label_three = label_cpt_atypical,
#'    label_four = label_cpt_typical,
#'    label_unknown = label_cpt_unknown,
#'    harmonise_label_one = "no chest pain",
#'    harmonise_label_two = "nonanginal",
#'    harmonise_label_three = "atypical",
#'    harmonise_label_four = "typical",
#'    harmonise_label_unknown = NA
#' )
#'
#' # Gives error of invalid typical chest pain input with partial match
#' chest_pain_type <- "Typical"
#' label_cpt_unknown <- c(NA, NaN)
#'
#' try(harmonise_four_labels(
#'    arg = chest_pain_type,
#'    label_one = label_cpt_no_chest_pain,
#'    label_two = label_cpt_nonanginal,
#'    label_three = label_cpt_atypical,
#'    label_four = label_cpt_typical,
#'    label_unknown = label_cpt_unknown,
#'    harmonise_label_one = "no chest pain",
#'    harmonise_label_two = "nonanginal",
#'    harmonise_label_three = "atypical",
#'    harmonise_label_four = "typical",
#'    harmonise_label_unknown = NA
#' ))
#'
#' # Gives error of invalid typical chest pain input without partial match
#' chest_pain_type <- "Something"
#' label_cpt_unknown <- c(NA, NaN)
#'
#' try(harmonise_four_labels(
#'    arg = chest_pain_type,
#'    label_one = label_cpt_no_chest_pain,
#'    label_two = label_cpt_nonanginal,
#'    label_three = label_cpt_atypical,
#'    label_four = label_cpt_typical,
#'    label_unknown = label_cpt_unknown,
#'    harmonise_label_one = "no chest pain",
#'    harmonise_label_two = "nonanginal",
#'    harmonise_label_three = "atypical",
#'    harmonise_label_four = "typical",
#'    harmonise_label_unknown = NA
#' ))
#'
#' # Gives error of invalid missing input of chest_pain_type
#' chest_pain_type <- NA
#' label_cpt_unknown <- c("NIL")
#'
#' try(harmonise_four_labels(
#'    arg = chest_pain_type,
#'    label_one = label_cpt_no_chest_pain,
#'    label_two = label_cpt_nonanginal,
#'    label_three = label_cpt_atypical,
#'    label_four = label_cpt_typical,
#'    label_unknown = label_cpt_unknown,
#'    harmonise_label_one = "no chest pain",
#'    harmonise_label_two = "nonanginal",
#'    harmonise_label_three = "atypical",
#'    harmonise_label_four = "typical",
#'    harmonise_label_unknown = NA
#' ))
#'
#'
#' @rdname harmonise_four_labels
#' @export
harmonise_four_labels <- function(
    arg,
    label_one,
    label_two,
    label_three,
    label_four,
    label_unknown,
    harmonise_label_one = "group_1",
    harmonise_label_two = "group_2",
    harmonise_label_three = "group_3",
    harmonise_label_four = "group_4",
    harmonise_label_unknown = NA,
    error_call = rlang::caller_env()
)
{
  # Ensure arg is valid and mapped to a unified group
  # (harmonise_label_one, harmonise_label_two,
  #  harmonise_label_three, harmonise_label_four,
  #  harmonise_label_unknown)
  label_all <- c(label_one, label_two, label_three, label_four, label_unknown)

  if (any(is.na(label_all))) {
    label_all[is.na(label_all)] <- "NA"
    arg <- arg_match0_allow_na(
      arg = arg,
      values = label_all,
      arg_nm = rlang::caller_arg(arg),
      error_call = error_call)
  } else if (is.null(arg) || is.na(arg)) {
    arg_match0_no_na_error_message(
      arg = arg,
      values = label_all,
      arg_nm = rlang::caller_arg(arg),
      error_call = error_call
    )
  } else {
    arg <- rlang::arg_match0(
      arg = arg,
      values = label_all,
      arg_nm = rlang::caller_arg(arg),
      error_call = error_call
    )
  }

  arg <- dplyr::case_when(
    arg %in% label_one ~ harmonise_label_one,
    arg %in% label_two ~ harmonise_label_two,
    arg %in% label_three ~ harmonise_label_three,
    arg %in% label_four ~ harmonise_label_four,
    .default = harmonise_label_unknown
  )

  return(arg)

}

#' @title Round To Nearest Digit
#' @description A function that does
#' symmetric rounding to the nearest digits.
#' @param number Input numeric value
#' @param digits Input integer indicating the number of
#' decimal places to be used. By default, it rounds
#' off to the nearest integer.
#' Default: 0
#' @return A numeric value rounded off to a number of decimal places specified in
#' the input \code{digits}.
#' @examples
#' round_to_nearest_digit(0.5)
#' round_to_nearest_digit(1.5)
#' round_to_nearest_digit(-0.5)
#' round_to_nearest_digit(-1.5)
#' round_to_nearest_digit(1021.125, digits = 2)
#' @rdname round_to_nearest_digit
#' @export
round_to_nearest_digit <- function(number, digits = 0) {
  # https://stackoverflow.com/questions/12688717/round-up-from-5
  posneg <- sign(number)
  number <- abs(number) * 10^digits
  number <- number + 0.5 + sqrt(.Machine$double.eps)
  number <- trunc(number)
  number <- number / 10 ^ digits
  number <- number * posneg
  return(number)
}

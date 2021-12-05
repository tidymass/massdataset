#' Determine the context of subsequent manipulations
#' @param .data A mass_dataset class object
#' @param what What should get activated? Possible values are `sample_info`,
#' `expression_data`, `variable_info`, `sample_info_note` or
#' `variable_info_note`.
#' @return A tidyclas class object
#' @export
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("sample_info_note")
#' data("variable_info")
#' data("variable_info_note")
#' 
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'     sample_info_note = sample_info_note,
#'     variable_info_note = variable_info_note
#'   )
#' object
#' object@activated
#' object =
#'   activate_mass_dataset(.data = object, what = "sample_info")
#' object@activated

activate_mass_dataset <- function(.data,
                              what = c(
                                "sample_info",
                                "variable_info",
                                "expression_data",
                                "sample_info_note",
                                "variable_info_note"
                              )) {
  what = match.arg(what)
  .data@activated = what
  return(.data)
}

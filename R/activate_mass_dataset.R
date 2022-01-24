#' @title activate_mass_dataset
#' @description Determine the context of subsequent manipulations
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param .data (required) A mass_dataset class object
#' @param what (required) What should get be activated? 
#' Possible values are `sample_info`,
#' `expression_data`, `variable_info`, `sample_info_note` `annotation_table`,or
#' `variable_info_note`.
#' @return A mass_dataset class object
#' @export
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#'
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info
#'   )
#' object
#' object@activated
#' object =
#'   activate_mass_dataset(.data = object, what = "sample_info")
#' object@activated

activate_mass_dataset <-
  function(.data,
           what = c(
             "sample_info",
             "variable_info",
             "expression_data",
             "sample_info_note",
             "variable_info_note",
             "annotation_table"
           )) {
    check_object_class(object = .data, class = "mass_dataset")
    what = match.arg(what)
    .data@activated = what
    return(.data)
  }

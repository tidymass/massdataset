#' Activate a Specific Dataset
#'
#' Activates a dataset within the mass dataset object for further analysis.
#'
#' @param .data A `mass_dataset` object.
#' @param what A character string specifying which dataset to activate.
#'   The available options are:
#'   \itemize{
#'     \item "sample_info": Activates the sample information dataset.
#'     \item "variable_info": Activates the variable information dataset.
#'     \item "expression_data": Activates the expression data.
#'     \item "annotation_table": Activates the annotation table.
#'   }
#'   By default, it activates the "sample_info" dataset.
#'
#' @details 
#' This function allows users to activate a specific dataset within the 
#' `mass_dataset` object. The activated dataset can be then used in subsequent
#' analysis steps.
#'
#' @return 
#' Returns the `mass_dataset` object with the specified dataset activated.
#'
#' @author Xiaotao Shen \email{shenxt1990@@outlook.com}
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
             "annotation_table"
           )) {
    check_object_class(object = .data, class = "mass_dataset")
    what <- match.arg(what)
    slot(object = .data, name = "activated") <- what
    # .data@activated = what
    return(.data)
  }

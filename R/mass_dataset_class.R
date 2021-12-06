#' @title create_mass_dataset
#' @description Create mass_dataset object.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param expression_data MS1 peak table name. Columns are samples and rows are variables.
#' @param sample_info Sample information name.
#' @param variable_info MS1 peak table name. Columns are samples and rows are variables.
#' @param sample_info_note Sample information name.
#' @param variable_info_note Sample information name.
#' @return A mass_dataset-class object.
#' @export
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("sample_info_note")
#' data("variable_info")
#' data("variable_info_note")
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'     sample_info_note = sample_info_note,
#'     variable_info_note = variable_info_note
#'   )
#'   object

create_mass_dataset =
  function(expression_data,
           sample_info,
           variable_info,
           sample_info_note,
           variable_info_note) {
    check_result <-
      check_mass_dataset(
        expression_data = expression_data,
        sample_info = sample_info,
        variable_info = variable_info,
        sample_info_note = sample_info_note,
        variable_info_note = variable_info_note
      )
    
    if (stringr::str_detect(check_result, "error")) {
      stop(check_result)
    }
    
    if (missing(sample_info_note)) {
      sample_info_note = data.frame()
    }
    
    if (missing(variable_info_note)) {
      variable_info_note = data.frame()
    }
    
    process_info = list()
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "create_mass_dataset()",
      parameter = list("no" = "no"),
      time = Sys.time()
    )
    
    process_info$Creation = parameter
    
    object <- new(
      Class = "mass_dataset",
      expression_data = expression_data,
      ms2_data = data.frame(),
      sample_info = sample_info,
      variable_info = variable_info,
      sample_info_note = sample_info_note,
      variable_info_note = variable_info_note,
      process_info = process_info,
      version = "0.9.1"
    )
    invisible(object)
  }


##S4 class for function mass_dataset-class
#' An S4 class that stores the MS dataset
#' @name mass_dataset
#' @docType class
#' @slot expression_data expression_data
#' @slot ms2_data ms2_data
#' @slot sample_info sample_info
#' @slot variable_info variable_info
#' @slot sample_info_note sample_info_note
#' @slot variable_info_note variable_info_note
#' @slot process_info process_info
#' @slot version version
#' @slot activated activated
#' @export
setClass(
  Class = "mass_dataset",
  representation(
    expression_data = "data.frame",
    ms2_data = "list",
    sample_info = "data.frame",
    variable_info = "data.frame",
    sample_info_note = "data.frame",
    variable_info_note = "data.frame",
    process_info = "list",
    version = "character",
    activated = "character"
  )
)

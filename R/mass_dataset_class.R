#' @title create_mass_dataset
#' @description Create the mass_dataset object.
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param expression_data MS1 peak table name.
#' \url{https://tidymass.github.io/massdataset/articles/data_import_and_export.html}
#' @param sample_info Sample information name.
#' \url{https://tidymass.github.io/massdataset/articles/data_import_and_export.html}
#' @param variable_info MS1 peak table name.
#' Columns are samples and rows are variables.
#' \url{https://tidymass.github.io/massdataset/articles/data_import_and_export.html}
#' @param sample_info_note Sample information name.
#' \url{https://tidymass.github.io/massdataset/articles/data_import_and_export.html}
#' @param variable_info_note Sample information name.
#' \url{https://tidymass.github.io/massdataset/articles/data_import_and_export.html}
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
#' object

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
      sample_info_note = data.frame(name = colnames(sample_info),
                                    meaning = colnames(sample_info))
    }
    
    if (missing(variable_info_note)) {
      variable_info_note = data.frame(name = colnames(variable_info),
                                      meaning = colnames(variable_info))
    }
    
    process_info = list()
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "create_mass_dataset()",
      parameter = list("no" = "no"),
      time = Sys.time()
    )
    
    process_info$create_mass_dataset = parameter
    
    object <- new(
      Class = "mass_dataset",
      expression_data = expression_data,
      ms2_data = list(),
      annotation_table = data.frame(),
      sample_info = sample_info,
      variable_info = variable_info,
      sample_info_note = sample_info_note,
      variable_info_note = variable_info_note,
      process_info = process_info,
      version = massdataset_version
    )
    invisible(object)
  }


##S4 class for function mass_dataset-class
#' An S4 class that stores the MS dataset
#' @docType class
#' @slot expression_data expression data
#' @slot ms2_data ms2 data
#' @slot annotation_table annotation table
#' @slot sample_info sample info
#' @slot variable_info variable info
#' @slot sample_info_note sample info note
#' @slot variable_info_note variable info note
#' @slot process_info process info
#' @slot version version
#' @slot activated activated
#' @exportClass mass_dataset

setClass(
  Class = "mass_dataset",
  representation(
    expression_data = "data.frame",
    ms2_data = "list",
    annotation_table = "data.frame",
    sample_info = "data.frame",
    variable_info = "data.frame",
    sample_info_note = "data.frame",
    variable_info_note = "data.frame",
    process_info = "list",
    version = "character",
    activated = "character"
  ),
  validity = check_mass_dataset_class
)

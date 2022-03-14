
#' @title check_mass_dataset_class
#' @description Check mass_dataset class object.
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object mass_dataset class.
#' @return Notice of data checking.
#' @export
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object <-
#'   create_mass_dataset(expression_data = expression_data,
#'                       sample_info = sample_info,
#'                       variable_info = variable_info)
#'   check_mass_dataset_class(object)

check_mass_dataset_class <-
  function(object) {
    errors <- character()
    ##check variable_info format
    if (all(colnames(object@variable_info) != "variable_id")) {
      msg <- "variable_info must have variable_id."
      errors <- c(errors, msg)
    }
    
    if (sum(duplicated(object@variable_info$variable_id)) > 0) {
      msg <- "variable_id has duplicated items."
      errors <- c(errors, msg)
    }
    
    ##check sample_info format
    if (all(colnames(object@sample_info) != "sample_id")) {
      msg <- "sample_info must have sample_id."
      errors <- c(errors, msg)
    }
    
    if (sum(duplicated(object@sample_info$sample_id)) > 0) {
      msg <- "sample_id has duplicated items."
      errors <- c(errors, msg)
    }
    
    if (all(colnames(object@sample_info) != "class")) {
      msg <- "sample_info must have class."
      errors <- c(errors, msg)
    }
    
    # if (any(is.na(object@sample_info$class))) {
    #   msg <- "class should not have NA."
    #   errors <- c(errors, msg)
    # }
    
    ##check sample_info_note format
    if (nrow(object@sample_info_note) > 0) {
      if (all(colnames(object@sample_info_note) != "name")) {
        msg <- "sample_info_note must have column: name."
        errors <- c(errors, msg)
      }
      
      if (all(colnames(object@sample_info_note) != "meaning")) {
        msg <- "sample_info_note must have column: meaning"
        errors <- c(errors, msg)
      }
    }
    
    ##check variable_info_note format
    if (nrow(object@variable_info_note) > 0) {
      if (all(colnames(object@variable_info_note) != "name")) {
        msg <- "variable_info_note must have column: name."
        errors <- c(errors, msg)
      }
      
      if (all(colnames(object@variable_info_note) != "meaning")) {
        msg <- "variable_info_note must have column: meaning"
        errors <- c(errors, msg)
      }
    }
    
    ###expression_data and sample_info
    if (ncol(object@expression_data) != nrow(object@sample_info)) {
      msg <-
        "expression_data's column number should be same with sample_info's row number."
      errors <- c(errors, msg)
    } else{
      if (sum(colnames(object@expression_data) != object@sample_info$sample_id) > 0) {
        msg <-
          "expression_data's column names must be identical with sample_info's sample_id."
        errors <- c(errors, msg)
      }
    }
    
    ###expression_data and variable_info
    if (nrow(object@expression_data) != nrow(object@variable_info)) {
      msg <-
        "expression_data's row number should be same with variable_info's row number."
      errors <- c(errors, msg)
    } else{
      if (sum(rownames(object@expression_data) != object@variable_info$variable_id) > 0) {
        msg <-
          "expression_data's row names must be identical with variable_info's variable_id"
        errors <- c(errors, msg)
      }
    }
    
    
    ###sample_info and sample_info_note
    if (nrow(object@sample_info_note) > 0) {
      if (ncol(object@sample_info) != nrow(object@sample_info_note)) {
        msg <-
          "sample_info's column number should be same with sample_info_note's row number."
        errors <- c(errors, msg)
      } else{
        if (sum(colnames(object@sample_info) != object@sample_info_note$name) > 0) {
          msg <-
            "sample_info's column names must be identical with sample_info_note's name."
          errors <- c(errors, msg)
        }
      }
    }
    
    ###variable_info and variable_info_note
    if (nrow(object@variable_info_note) > 0) {
      if (ncol(object@variable_info) != nrow(object@variable_info_note)) {
        msg <-
          "variable_info's column number should be same with variable_info_note's row number."
        errors <- c(errors, msg)
      } else{
        if (sum(colnames(object@variable_info) != object@variable_info_note$name) > 0) {
          msg <-
            "variable_info's column names must be identical with variable_info_note's name."
          errors <- c(errors, msg)
        }
      }
    }
    
    if (length(errors) == 0)
      TRUE
    else
      errors
  }


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

create_mass_dataset <-
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
      sample_info_note <-
        data.frame(
          name = colnames(sample_info),
          meaning = colnames(sample_info),
          check.names = FALSE
        )
    }
    
    if (missing(variable_info_note)) {
      variable_info_note <-
        data.frame(name = colnames(variable_info),
                   meaning = colnames(variable_info),
                   check.names = FALSE)
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
      version = as.character(utils::packageVersion(pkg = "massdataset"))
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

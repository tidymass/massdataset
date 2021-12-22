#' @title check_mass_dataset
#' @description Check data format.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param expression_data expression_data.
#' @param sample_info sample_info.
#' @param variable_info variable_info.
#' @param sample_info_note sample_info_note.
#' @param variable_info_note variable_info_note.
#' @return Notice of data checking.
#' @export
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#'   check_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'   )

check_mass_dataset =
  function(expression_data,
           sample_info,
           variable_info,
           sample_info_note,
           variable_info_note) {
    # browser()
    if (missing(expression_data) |
        missing(sample_info) | missing(variable_info)) {
      check_result = "error: expression_data, sample_info and variable_info should be provided."
      return(check_result)
    }
    
    ##check expression_data format
    if (all(!stringr::str_detect(class(expression_data), "data.frame"))) {
      check_result = "error: expression_data must be a data.frame."
      return(check_result)
    }
    
    ##check variable_info format
    if (all(!stringr::str_detect(class(variable_info), "data.frame"))) {
      check_result = "error: variable_info must be a data.frame."
      return(check_result)
    } else{
      if (all(colnames(variable_info) != "variable_id")) {
        check_result = "error: variable_info must have variable_id."
        return(check_result)
      }
      
      if (sum(duplicated(variable_info$variable_id)) > 0) {
        check_result = "error: variable_id has duplicated items."
        return(check_result)
      }
      
    }
    
    ##check sample_info format
    if (all(!stringr::str_detect(class(sample_info), "data.frame"))) {
      check_result = "error: sample_info must be a data.frame."
      return(check_result)
    } else{
      if (all(colnames(sample_info) != "sample_id")) {
        check_result = "error: sample_info must have sample_id."
        return(check_result)
      }
      
      if (sum(duplicated(sample_info$sample_id)) > 0) {
        check_result = "error: sample_id has duplicated items."
        return(check_result)
      }
      
      if (all(colnames(sample_info) != "class")) {
        check_result = "error: sample_info must have class."
        return(check_result)
      }
      
      if (any(is.na(sample_info$class))) {
        check_result = "error: class should not have NA."
        return(check_result)
      }
      
      # if (all(sample_info$class != "Subject")) {
      #   check_result = "error: class should have at least one Subject sample."
      #   return(check_result)
      # }
      
      # if (any(sample_info$class %in% c("QC", "Blank", "QC_DL"))) {
      #   check_result = "warning: Blank should be named as Blank, QC should be named as
      #   QC and QC_DL should be named as QC_DL."
      # }
    }
    
    ##check sample_info_note format
    if (!missing(sample_info_note)) {
      if (all(!stringr::str_detect(class(sample_info_note), "data.frame"))) {
        check_result = "error: sample_info_note must be a data.frame."
        return(check_result)
      } else{
        if (all(colnames(sample_info_note) != "name")) {
          check_result = "error: sample_info_note must have column: name."
          return(check_result)
        }
        
        if (all(colnames(sample_info_note) != "meaning")) {
          check_result = "error: sample_info_note must have column: meaning"
          return(check_result)
        }
      }
    }
    
    ##check variable_info_note format
    if (!missing(variable_info_note)) {
      if (all(!stringr::str_detect(class(variable_info_note), "data.frame"))) {
        check_result = "error: variable_info_note must be a data.frame."
        return(check_result)
      } else{
        if (all(colnames(variable_info_note) != "name")) {
          check_result = "error: variable_info_note must have column: name."
          return(check_result)
        }
        
        if (all(colnames(variable_info_note) != "meaning")) {
          check_result = "error: variable_info_note must have column: meaning"
          return(check_result)
        }
      }
    }
    
    ###expression_data and sample_info
    if (ncol(expression_data) != nrow(sample_info)) {
      check_result = "error: expression_data's column number should be same with sample_info's row number."
      return(check_result)
    } else{
      if (sum(colnames(expression_data) != sample_info$sample_id) > 0) {
        check_result = "error: expression_data's column names must be identical with sample_info's sample_id."
        return(check_result)
      }
    }
    
    ###expression_data and variable_info
    if (nrow(expression_data) != nrow(variable_info)) {
      check_result = "error: expression_data's row number should be same with variable_info's row number."
      return(check_result)
    } else{
      if (sum(rownames(expression_data) != variable_info$variable_id) > 0) {
        check_result = "error: expression_data's row names must be identical with variable_info's variable_id"
        return(check_result)
      }
    }
    
    
    ###sample_info and sample_info_note
    if (!missing(sample_info_note)) {
      if (ncol(sample_info) != nrow(sample_info_note)) {
        check_result = "error: sample_info's column number should be same with sample_info_note's row number."
        return(check_result)
      } else{
        if (sum(colnames(sample_info) != sample_info_note$name) > 0) {
          check_result = "error: sample_info's column names must be identical with sample_info_note's name."
          return(check_result)
        }
      }
    }
    
    
    ###variable_info and variable_info_note
    if (!missing(variable_info_note)) {
      if (ncol(variable_info) != nrow(variable_info_note)) {
        check_result = "error: variable_info's column number should be same with variable_info_note's row number."
        return(check_result)
      } else{
        if (sum(colnames(variable_info) != variable_info_note$name) > 0) {
          check_result = "error: variable_info's column names must be identical with variable_info_note's name."
          return(check_result)
        }
      }
    }
    return("all good.")
  }


#' @title check_object_class
#' @description check_object_class
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param object object
#' @param class object should be what class
#' @return Notice of data checking.
#' @export
check_object_class = function(object,
                              class = c("mass_dataset", 
                                        "tidymass_parameter", 
                                        "databaseClass",
                                        "metIdentifyClass")) {
  class = match.arg(class)
  if (class(object)[1] != class) {
    stop("Only support ", class)
  }
}

# tinytools::setwd_project()
# load("demo_data/object")
# 
# object


#' @title extract_expression_data
#' @description Extract expression data.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object tidymass class object.
#' @return A data frame.
#' @export

# extract_expression_data(object = object)

extract_expression_data = function(object) {
  expression_data = object@expression_data %>%
    as.data.frame()
  expression_data
}


#' @title extract_sample_info
#' @description Extract sample information.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object tidymass class object.
#' @return A data frame.
#' @export

extract_sample_info = function(object) {
  sample_info = object@sample_info %>%
    as.data.frame()
  sample_info
}


#' @title extract_variable_info
#' @description Extract variable information.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object tidymass class object.
#' @return A data frame.
#' @export

extract_variable_info = function(object) {
  variable_info = object@variable_info %>%
    as.data.frame()
  variable_info
}


#' @title extract_variable_info_note
#' @description Extract variable information note.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object tidymass class object.
#' @return A data frame.
#' @export

extract_variable_info_note = function(object) {
  variable_info_note = object@variable_info_note %>%
    as.data.frame()
  variable_info_note
}


#' @title extract_sample_info_note
#' @description Extract sample information note.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object tidymass class object.
#' @return A data frame.
#' @export

extract_sample_info_note = function(object) {
  sample_info_note = object@sample_info_note %>%
    as.data.frame()
  sample_info_note
}
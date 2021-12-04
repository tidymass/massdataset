#' @title get_sample_number
#' @description Number of samples
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object tidymass class object.
#' @return A numeric.
#' @export
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object =
#'   create_tidymass_class(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'   )
#'  get_sample_number(object = object)

get_sample_number = function(object) {
  object@expression_data %>%
    as.data.frame() %>%
    ncol()
}


#' @title get_variable_number
#' @description Number of variables
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object tidymass class object.
#' @return A numeric.
#' @export
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object =
#'   create_tidymass_class(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'   )
#'  get_variable_number(object = object)

get_variable_number = function(object) {
  object@expression_data %>%
    as.data.frame() %>%
    nrow()
}




#' @title get_sample_id
#' @description Get sample names
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object tidymass class object.
#' @return A character vector
#' @export
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object =
#'   create_tidymass_class(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'   )
#'  get_sample_id(object = object)

get_sample_id = function(object) {
  object@expression_data %>%
    as.data.frame() %>%
    colnames()
}


#' @title get_variable_id
#' @description Get sample names
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object tidymass class object.
#' @return A character vector
#' @export
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' library(tidyverse)
#' object =
#'   create_tidymass_class(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'   )
#'  get_variable_id(object = object) %>% head()

get_variable_id = function(object) {
  object@expression_data %>%
    as.data.frame() %>%
    rownames()
}


#' @title get_mv_number
#' @description Get missing value number/percentage in expression
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object tidymass class object.
#' @param by total: Missing value number in total. sample: Missing value number in each sample.
#' variable: Missing value number in each variable.
#' @param show_by number: missing value number. percentage: ratio.
#' @return A numeric vector
#' @export
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object =
#'   create_tidymass_class(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'   )
#' object 
#' head(get_variable_id(object = object))
#' get_mv_number(object)
#' get_mv_number(object, by = "sample")
#' head(get_mv_number(object, by = "variable", "percentage"))

get_mv_number = function(object,
                         by = c("total", "sample", "variable"),
                         show_by = c("number", "percentage")) {
  by = match.arg(by)
  show_by = match.arg(show_by)
  
  expression_data =
    object@expression_data %>%
    as.data.frame()
  
  total_mv_number =
    sum(is.na(expression_data))
  
  sample_mv_number =
    apply(expression_data, 2, function(x) {
      sum(is.na(x))
    })
  
  variable_mv_number =
    apply(expression_data, 1, function(x) {
      sum(is.na(x))
    })
  
  if (by == "total") {
    if (show_by == "number") {
      return(total_mv_number)
    } else{
      return(total_mv_number / (nrow(expression_data) * ncol(expression_data)))
    }
  }
  
  if (by == "sample") {
    if (show_by == "number") {
      return(sample_mv_number)
    } else{
      return(sample_mv_number / nrow(expression_data))
    }
  }
  
  if (by == "variable") {
    if (show_by == "number") {
      return(variable_mv_number)
    } else{
      return(variable_mv_number / ncol(expression_data))
    }
  }
}

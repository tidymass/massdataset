# tinytools::setwd_project()
# load("demo_data/object")
# 
# object


#' @title get_sample_number
#' @description Number of samples
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object tidymass class object.
#' @return A numeric.
#' @export

# get_sample_number(object = object)

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

# get_variable_number(object = object)

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

# get_sample_id(object = object)

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

# get_variable_id(object = object)

get_variable_id = function(object) {
  object@expression_data %>%
    as.data.frame() %>% 
    rownames()
}


# tinytools::setwd_project()
# load("demo_data/object")
# 
# object


#' @title filter_samples
#' @description Number of samples
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object tidymass class object.
#' @return A tidymass-class object
#' @export

# filter_samples(object = object)

filter_samples = 
  function(object) {
  object@expression_data %>%
    as.data.frame() %>%
    ncol()
}





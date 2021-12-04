#' @title Filter variables based on the conditions
#' @description Filter variables based on the conditions
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) tidymass class object.
#' @param flist (required) A function or list of functions that take a vector
#' of abundance values and return a logical. 
#' @param prune (optional) A logical. Default \code{FALSE}. If \code{TRUE}, then
#'  the function returns the pruned tidymass-class object, rather
#'  than the logical vector of variables that passed the filter.
#' @param apply_to what variables you want to apply this function. 
#' Default is "all". If you 
#'  only want to apply to specific variables, 
#'  please set it as a vector of sample names. Other
#'  variables will be set as TRUE.
#' @param according_to_samples What samples used to filter variables.
#' Default is "all". If you
#' want to use only several samples, provide they names as a vector.
#' @return A logical vector equal to the number of variables in tidymass-class.
#'  Alternatively, if \code{prune==TRUE}, the pruned tidymass-class
#'  object is returned instead.
#' @export
#' @examples
#' library(tidyverse)
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' 
#' object =
#'   create_tidymass_class(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'   )
#' object
#' 
#' ####Filter variables which have more than 50% MVs in all samples.
#' library(tidyverse)
#' filter_variables(object, function(x) {
#'   sum(is.na(x)) / length(x) < 0.5
#' }, prune = FALSE) %>%
#'   head()
#' 
#' filter_variables(object, function(x) {
#'   sum(is.na(x)) / length(x) < 0.5
#' },
#' prune = TRUE)
#' 
#' ####Filter variables which have more than 50% MVs in only QC samples.
#' filter_variables(
#'   object,
#'   flist = function(x) {
#'     sum(is.na(x)) / length(x) < 0.5
#'   },
#'   prune = TRUE,
#'   according_to_samples =
#'     get_sample_id(object)[extract_sample_info(object)$class == "QC"]
#' )
#' 
#' ####Filter variables which have more than 50% MVs in QC or subject samples.
#' idx1 =
#'   filter_variables(
#'     object,
#'     flist = function(x) {
#'       sum(is.na(x)) / length(x) < 0.5
#'     },
#'     prune = FALSE,
#'     according_to_samples =
#'       get_sample_id(object)[extract_sample_info(object)$class == "QC"]
#'   )
#' 
#' idx2 =
#'   filter_variables(
#'     object,
#'     flist = function(x) {
#'       sum(is.na(x)) / length(x) < 0.5
#'     },
#'     prune = FALSE,
#'     according_to_samples =
#'       get_sample_id(object)[extract_sample_info(object)$class == "Subject"]
#'   )
#' 
#' idx =
#'   which(idx1 | idx2)
#' 
#' object2 = object[idx,]
#' 
#' object2
#' 
#' ####filter variables with RSD (in QC samples) < 30
#' object3 =
#' filter_variables(
#'   object = object,
#'   flist = function(x) {
#'     rsd = sd(x) * 100 / mean(x)
#'     rsd = ifelse(is.na(rsd), 100, rsd)
#'     rsd < 30
#'   },
#'   apply_to = "all",
#'   prune = TRUE,
#'   according_to_samples = get_sample_id(object)[extract_sample_info(object)$class == "QC"]
#' )
#' 
#' object3

filter_variables =
  function(object, 
           flist, 
           prune = TRUE,
           apply_to = "all",
           according_to_samples = "all"
           ) {
    variable_id = get_variable_id(object)
    if(any(apply_to == "all")){
      apply_to = variable_id
    }else{
      apply_to = variable_id[variable_id %in% apply_to]
    }
    
    if(length(apply_to) == 0){
      stop("All the variables you provide in apply_to are not in the object.
           Please check.")
    }
    
    sample_id = get_sample_id(object)
    
    if(any(according_to_samples == "all")){
      according_to_samples = sample_id
    }else{
      according_to_samples = sample_id[sample_id %in% according_to_samples]
    }
    
    if(length(according_to_samples) == 0){
      stop("All the samples you provide in according_to_samples are not in the object.
           Please check.")
    }
    
    expression_data =
      object@expression_data %>%
      as.data.frame()

    result =
      expression_data[,according_to_samples] %>%
      apply(1, flist)
    
    result[!names(result) %in% apply_to] = TRUE
    
    ####add parameters
    process_info = object@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "filter_variables()",
      parameter = list("flist" = flist, 
                       prune = prune,
                       apply_to = apply_to,
                       according_to_samples = according_to_samples),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "Variable_filtering")) {
      process_info$Variable_filtering = parameter
    }else{
      process_info$Variable_filtering = c(process_info$Variable_filtering,
                                          parameter)  
    }
    
    object@process_info = process_info
    
    if (prune) {
      idx = which(result)
      object@expression_data = object@expression_data[idx, , drop = FALSE]
      object@variable_info = object@variable_info[idx,,drop = FALSE]
      return(object)
    } else{
      return(result)
    }
  }

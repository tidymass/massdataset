# tinytools::setwd_project()
# load("demo_data/object")
#
# object

#' @title Filter samples based on the conditions
#' @description Filter samples based on the conditions
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @param flist (required) A function or list of functions that take a vector
#' of abundance values and return a logical.
#' @param prune (optional) A logical. Default \code{FALSE}. If \code{TRUE}, then
#'  the function returns the pruned mass_dataset-class object, rather
#'  than the logical vector of samples that passed the filter.
#' @param apply_to what samples you want to apply this function. default is "all". If you
#'  only want to apply to specific samples, please set it as a vector of sample names. Other
#'  samples will be set as TRUE.
#' @return A logical vector equal to the number of samples in mass_dataset-class.
#'  Alternatively, if \code{prune==TRUE}, the pruned mass_dataset-class
#'  object is returned instead.
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
#'     variable_info = variable_info,
#'   )
#'  
#' filter_samples(object, function(x) {
#'   sum(is.na(x)) / length(x) < 0.4
#' })
#' 
#' filter_samples(object, function(x) {
#'   sum(is.na(x)) / length(x) < 0.4
#' }, prune = FALSE)
#' 
#' ##only apply to Subject sample
#' object2 =
#' filter_samples(
#'  object = object,
#'  flist = function(x) {
#'    sum(is.na(x))/length(x) < 0.2
#'  },
#'  prune = TRUE,
#'  apply_to = get_sample_id(object)[extract_sample_info(object)$class == "Subject"]
#' )
#' 
#' object2

filter_samples =
  function(object,
           flist,
           prune = TRUE,
           apply_to = "all") {
    sample_id = get_sample_id(object)
    if (any(apply_to == "all")) {
      apply_to = sample_id
    } else{
      apply_to = sample_id[sample_id %in% apply_to]
    }
    
    if (length(apply_to) == 0) {
      stop("All the samples you provide in apply_to are not in the object.
           Please check.")
    }
    
    expression_data =
      object@expression_data %>%
      as.data.frame()
    
    result =
      expression_data %>%
      apply(2, flist)
    
    result[!names(result) %in% apply_to] = TRUE
    
    process_info = object@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "filter_samples()",
      parameter = list("flist" = flist, 
                       prune = prune,
                       apply_to = apply_to),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "Sample_filtering")) {
      process_info$Sample_filtering = parameter
    }else{
      process_info$Sample_filtering = c(process_info$Sample_filtering, 
                                       parameter)  
    }
    
    object@process_info = process_info
    
    
    if (prune) {
      idx = which(result)
      object@expression_data = object@expression_data[, idx, drop = FALSE]
      object@sample_info = object@sample_info[idx, , drop = FALSE]
      return(object)
    } else{
      return(result)
    }
  }

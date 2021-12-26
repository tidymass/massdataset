#' @title Calculate RSD for features
#' @description Calculate RSD for features
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @param according_to_samples (required) What samples used to filter variables.
#' Default is "all". If you
#' want to use only several samples, provide they names as a vector.
#' @return A logical vector equal to the number of variables in mass_dataset-class.
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
#' object
#' 
#' ##calculate RSDs according to all the samples
#' object =
#'   mutate_rsd(object = object)
#' 
#' object
#' 
#' head(extract_variable_info(object))
#' 
#' ##calculate RSDs according to only QC samples
#' object =
#'   mutate_rsd(object = object,
#'                 according_to_samples =
#'               get_sample_id(object)[extract_sample_info(object)$class == "QC"])
#' 
#' object
#' 
#' head(extract_variable_info(object))

mutate_rsd =
  function(object, 
           according_to_samples = "all") {
    
    variable_id = get_variable_id(object)
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
    
    rsd =
      expression_data[,according_to_samples] %>%
      apply(1, function(x){
        sd(x, na.rm = TRUE) * 100/mean(x, na.rm = TRUE)
      })
    
    object@variable_info =
      data.frame(object@variable_info, rsd = rsd)
    
    
    ####variable_info_note
    new_variable_info_note = 
      data.frame(name = setdiff(colnames(object@variable_info), 
                                object@variable_info_note$name),
                 meaning = setdiff(colnames(object@variable_info), 
                                   object@variable_info_note$name))
    object@variable_info_note = 
      rbind(object@variable_info_note,
            new_variable_info_note)
    object@variable_info = object@variable_info[, object@variable_info_note$name] 
    
    process_info = object@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "mutate_rsd()",
      parameter = list("according_to_samples" = according_to_samples),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "mutate_rsd")) {
      process_info$mutate_rsd = parameter
    }else{
      process_info$mutate_rsd = c(process_info$mutate_rsd, 
                                       parameter)  
    }
    
    object@process_info = process_info
    
    return(object)
      
  }

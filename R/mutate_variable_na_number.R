#' @title Add NA number for each feature to variable_info
#' @description Add NA number for each feature to variable_info
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @param according_to_samples (required) What samples used to filter variables.
#' Default is "all". If you
#' want to use only several samples, provide their names as a vector.
#' @return A mass_dataset class object
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
#' ##calculate NA number according to all the samples
#' object2 =
#'   mutate_variable_na_number(object = object)
#' 
#' head(extract_variable_info(object))
#' head(extract_variable_info(object2))
#' 
#' ##calculate NA number according to only QC samples
#' object3 =
#'   mutate_variable_na_number(object = object2,
#'                 according_to_samples =
#'               get_sample_id(object)[extract_sample_info(object)$class == "QC"])
#' 
#' object3
#' 
#' head(extract_variable_info(object3))

mutate_variable_na_number =
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
    
    na_number = 
      expression_data[,according_to_samples] %>% 
      apply(1, function(x){
        sum(is.na(x))
      })
    
    object@variable_info =
      data.frame(object@variable_info, na_number = na_number)
    rownames(object@variable_info) = NULL
    process_info = object@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "mutate_variable_na_number()",
      parameter = list("according_to_samples" = according_to_samples),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "mutate_variable_na_number")) {
      process_info$mutate_variable_na_number = parameter
    }else{
      process_info$mutate_variable_na_number = c(process_info$mutate_variable_na_number, 
                                       parameter)  
    }
    
    object@process_info = process_info
    
    return(object)
      
  }



#' @title Add NA number for each feature to variable_info
#' @description Add NA number for each feature to variable_info
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @param according_to_samples (required) What samples used to filter variables.
#' Default is "all". If you
#' want to use only several samples, provide their names as a vector.
#' @return A mass_dataset class object
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
#' ##calculate NA frequency according to all the samples
#' object2 =
#'   mutate_variable_na_freq(object = object)
#' 
#' head(extract_variable_info(object))
#' head(extract_variable_info(object2))
#' 
#' ##calculate NA number according to only QC samples
#' object3 =
#'   mutate_variable_na_freq(object = object2,
#'                 according_to_samples =
#'               get_sample_id(object)[extract_sample_info(object)$class == "QC"])
#' 
#' object3
#' 
#' head(extract_variable_info(object3))

mutate_variable_na_freq =
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
    
    na_freq = 
      expression_data[,according_to_samples] %>% 
      apply(1, function(x){
        sum(is.na(x))/length(according_to_samples)
      })
    
    object@variable_info =
      data.frame(object@variable_info, na_freq = na_freq)
    rownames(object@variable_info) = NULL
    
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
      function_name = "mutate_variable_na_freq()",
      parameter = list("according_to_samples" = according_to_samples),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "mutate_variable_na_freq")) {
      process_info$mutate_variable_na_freq = parameter
    }else{
      process_info$mutate_variable_na_freq = c(process_info$mutate_variable_na_freq, 
                                                 parameter)  
    }
    
    object@process_info = process_info
    
    return(object)
    
  }

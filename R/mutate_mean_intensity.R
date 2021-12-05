#' @title Add mean intensity for each feature to variable_info
#' @description Add mean intensity for each feature to variable_info
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @param according_to_samples (required) What samples used to filter variables.
#' Default is "all". If you
#' want to use only several samples, provide their names as a vector.
#' @param na.rm na.rm
#' @return A mass_dataset class object
#' @export
#' @examples
#' library(massdataset)
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
#' ##calculate mean intensity according to all the samples
#' object2 =
#'   mutate_mean_intensity(object = object, na.rm = TRUE)
#' 
#' object2
#' 
#' head(extract_variable_info(object))
#' head(extract_variable_info(object2))
#' 
#' ##calculate mean intensity according to only QC samples
#' object3 =
#'   mutate_mean_intensity(object = object2,
#'                 according_to_samples =
#'               get_sample_id(object)[extract_sample_info(object)$class == "QC"])
#' 
#' object3
#' 
#' head(extract_variable_info(object3))
#' ###remain variables with mean intensity (QC) /  mean intensity (Blank) > 3
#' qc_sample_name = 
#'   get_sample_id(object)[extract_sample_info(object)$class == "QC"]
#' blank_sample_name = 
#'   get_sample_id(object)[extract_sample_info(object)$class == "Blank"]
#' 
#' object4 =
#' object %>%
#'   mutate_mean_intensity(according_to_samples = qc_sample_name, 
#'                         na.rm = TRUE) %>% 
#'   mutate_mean_intensity(according_to_samples = blank_sample_name,
#'                         na.rm = TRUE) %>% 
#'   activate_mass_dataset(what = "variable_info") %>% 
#'   mutate(mean_intensity.1 = case_when(
#'     is.na(mean_intensity.1) ~ 0,
#'     TRUE ~ mean_intensity.1
#'   )) %>% 
#'   mutate(mean_intensity = case_when(
#'     is.na(mean_intensity) ~ 0,
#'     TRUE ~ mean_intensity
#'   )) %>% 
#'   mutate(qc_blank_ratio = mean_intensity.1 / mean_intensity) %>% 
#'   mutate(qc_blank_ratio = case_when(
#'     is.na(qc_blank_ratio) ~ 0,
#'     TRUE ~ qc_blank_ratio
#'   )) %>% 
#'   filter(qc_blank_ratio > 3)
#' 
#' object4
#' object4 %>% 
#'   extract_variable_info()


mutate_mean_intensity =
  function(object, 
           according_to_samples = "all",
           na.rm = TRUE) {
    
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
    
    mean_intensity = 
      expression_data[,according_to_samples] %>% 
      apply(1, function(x){
        mean(x, na.rm = na.rm)
      })
    
    object@variable_info =
      data.frame(object@variable_info, mean_intensity = mean_intensity)
    
    process_info = object@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "mutate_mean_intensity()",
      parameter = list("according_to_samples" = according_to_samples),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "mutate_mean_intensity")) {
      process_info$mutate_mean_intensity = parameter
    }else{
      process_info$mutate_mean_intensity = c(process_info$mutate_mean_intensity, 
                                       parameter)  
    }
    
    object@process_info = process_info
    
    return(object)
      
  }

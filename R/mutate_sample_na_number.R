#' @title Add NA number for each sample to sample_info
#' @description Add NA number for each sample to sample_info.
#' @docType methods
#' @rdname add_new_column-mass_dataset
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @param according_to_variables (required) What variables used to calculate 
#' NA number or percentage. Default is "all". If you
#' want to use only several variables, provide their names as a vector.
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
#'     variable_info = variable_info
#'   )
#' 
#' object
#' 
#' ##calculate NA number according to all the variables
#' object2 =
#'   mutate_sample_na_number(object = object)
#' 
#' head(extract_sample_info(object))
#' head(extract_sample_info(object2))
#' 
#' ##calculate NA number according to only variables with mz > 100
#' variable_id = 
#' object2 %>% 
#'   activate_mass_dataset(what = "variable_info") %>% 
#'   filter(mz > 100) %>%
#'   pull(variable_id)
#' 
#' object3 =
#'   mutate_sample_na_number(object = object2,
#'                 according_to_variables = variable_id)
#' 
#' object3
#' 
#' head(extract_sample_info(object3))


mutate_sample_na_number =
  function(object, 
           according_to_variables = "all") {
    
    check_object_class(object = object, class = "mass_dataset")
    
    variable_id = get_variable_id(object)
    sample_id = get_sample_id(object)
    
    if(any(according_to_variables == "all")){
      according_to_variables = variable_id
    }else{
      according_to_variables = variable_id[variable_id %in% according_to_variables]
    }
    
    if(length(according_to_variables) == 0){
      stop("All the variables you provide in according_to_variables are not in the object.
           Please check.")
    }
    
    expression_data =
      object@expression_data %>%
      as.data.frame()
    
    na_number = 
      expression_data[according_to_variables,] %>% 
      apply(2, function(x){
        sum(is.na(x))
      })
    
    
    new_column_name <- 
      check_column_name(object@sample_info , 
                        column.name = "na_number")
    
    object@sample_info =
      cbind(object@sample_info,
            na_number = na_number) %>%
      as.data.frame()
    
    colnames(object@sample_info)[ncol(object@sample_info)] <-
      new_column_name
    
    rownames(object@sample_info) = NULL
    
    
    ####variable_info_note
    new_sample_info_note <-
      data.frame(name = setdiff(colnames(object@sample_info), 
                                object@sample_info_note$name),
                 meaning = setdiff(colnames(object@sample_info), 
                                   object@sample_info_note$name),
                 check.names = FALSE)
    object@sample_info_note = 
      rbind(object@sample_info_note,
            new_sample_info_note)
    object@sample_info = object@sample_info[, object@sample_info_note$name] 
    
    
    process_info = object@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "mutate_sample_na_number()",
      parameter = list("according_to_variables" = according_to_variables),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "mutate_sample_na_number")) {
      process_info$mutate_sample_na_number = parameter
    }else{
      process_info$mutate_sample_na_number = c(process_info$mutate_sample_na_number, 
                                             parameter)  
    }
    
    object@process_info = process_info
    
    return(object)
    
  }




#' @title Add NA frequency for each sample to sample_info
#' @description Add NA frequency for each sample to sample_info
#' @docType methods
#' @rdname add_new_column-mass_dataset
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @param according_to_variables (required) What variables used to calculate 
#' NA number or percentage. Default is "all". If you
#' want to use only several variables, provide their names as a vector.
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
#'     variable_info = variable_info
#'   )
#' 
#' object
#' 
#' ##calculate NA frequency according to all the variables
#' object2 =
#'   mutate_sample_na_freq(object = object)
#' 
#' head(extract_sample_info(object))
#' head(extract_sample_info(object2))
#' 
#' ##calculate NA frequency according to only variables with mz > 100
#' variable_id =
#' object2 %>%
#'   activate_mass_dataset(what = "variable_info") %>%
#'   filter(mz > 100) %>%
#'   pull(variable_id)
#' 
#' object3 =
#'   mutate_sample_na_freq(object = object2,
#'                 according_to_variables = variable_id)
#' 
#' object3
#' 
#' head(extract_sample_info(object3))


mutate_sample_na_freq =
  function(object, 
           according_to_variables = "all") {
    
    check_object_class(object = object, class = "mass_dataset")
    
    variable_id = get_variable_id(object)
    sample_id = get_sample_id(object)
    
    if(any(according_to_variables == "all")){
      according_to_variables = variable_id
    }else{
      according_to_variables = variable_id[variable_id %in% according_to_variables]
    }
    
    if(length(according_to_variables) == 0){
      stop("All the variables you provide in according_to_variables are not in the object.
           Please check.")
    }
    
    expression_data =
      object@expression_data %>%
      as.data.frame()
    
    na_freq = 
      expression_data[according_to_variables,] %>% 
      apply(2, function(x){
        sum(is.na(x))/length(according_to_variables)
      })
    
    
    new_column_name <- 
      check_column_name(object@sample_info , 
                        column.name = "na_freq")
    
    object@sample_info =
      cbind(object@sample_info,
            na_freq = na_freq) %>%
      as.data.frame()
    
    colnames(object@sample_info)[ncol(object@sample_info)] <-
      new_column_name
    
    rownames(object@sample_info) = NULL
    process_info = object@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "mutate_sample_na_freq()",
      parameter = list("according_to_variables" = according_to_variables),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "mutate_sample_na_freq")) {
      process_info$mutate_sample_na_freq = parameter
    }else{
      process_info$mutate_sample_na_freq = c(process_info$mutate_sample_na_freq, 
                                                 parameter)  
    }
    
    object@process_info = process_info
    
    return(object)
    
  }

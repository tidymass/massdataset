#' Mutate Sample Zero Number in mass_dataset Object
#'
#' This function adds a new column to the `sample_info` slot of a mass_dataset object,
#' which contains the number of zero values for each sample according to the variables specified.
#' @param object A mass_dataset object.
#' @param according_to_variables A character vector specifying the variable IDs to consider when
#'                              calculating the number of zero values. Default is "all", which considers all variables.
#'
#' @return A modified mass_dataset object with an updated `sample_info` slot.
#'
#' @author Xiaotao Shen <shenxt1990@outlook.com>
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
#'   mutate_sample_zero_number(object = object)
#'
#' colnames(extract_sample_info(object))
#' colnames(extract_sample_info(object2))
#' object2@sample_info_note
#'
#' ##calculate NA number according to only variables with mz > 100
#' variable_id =
#' object2 %>%
#'   activate_mass_dataset(what = "variable_info") %>%
#'   filter(mz > 100) %>%
#'   pull(variable_id)
#'
#' object3 =
#'   mutate_sample_zero_number(object = object2,
#'                 according_to_variables = variable_id)
#'
#' object3
#'
#' head(extract_sample_info(object3))

mutate_sample_zero_number <-
  function(object,
           according_to_variables = "all") {
    check_object_class(object = object, class = "mass_dataset")
    
    variable_id <- get_variable_id(object)
    sample_id <- get_sample_id(object)
    
    if (any(according_to_variables == "all")) {
      according_to_variables <- variable_id
    } else{
      according_to_variables <-
        variable_id[variable_id %in% according_to_variables]
    }
    
    if (length(according_to_variables) == 0) {
      stop(
        "All the variables you provide in according_to_variables are not in the object.
           Please check."
      )
    }
    
    expression_data <-
      object@expression_data %>%
      as.data.frame()
    
    zero_number <-
      expression_data[according_to_variables, ] %>%
      apply(2, function(x) {
        sum(x == 0)
      })
    
    new_column_name <-
      check_column_name(object@sample_info ,
                        column.name = "zero_number")
    
    object@sample_info <-
      cbind(object@sample_info,
            zero_number = zero_number) %>%
      as.data.frame()
    
    colnames(object@sample_info)[ncol(object@sample_info)] <-
      new_column_name
    
    rownames(object@sample_info) <- NULL
    
    ####sample_info_note
    object <- update_sample_info(object)
    # new_sample_info_note <-
    #   data.frame(name = setdiff(colnames(object@sample_info),
    #                             object@sample_info_note$name),
    #              meaning = setdiff(colnames(object@sample_info),
    #                                object@sample_info_note$name),
    #              check.names = FALSE)
    # object@sample_info_note <-
    #   rbind(object@sample_info_note,
    #         new_sample_info_note)
    # object@sample_info <-
    # object@sample_info[, object@sample_info_note$name, drop = FALSE]
    
    process_info <- object@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "mutate_sample_zero_number()",
      parameter = list("according_to_variables" = according_to_variables),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "mutate_sample_zero_number")) {
      process_info$mutate_sample_zero_number <- parameter
    } else{
      process_info$mutate_sample_zero_number <-
        c(process_info$mutate_sample_zero_number,
          parameter)
    }
    
    object@process_info <- process_info
    
    return(object)
  }

#' Mutate Sample Zero Frequency in mass_dataset Object
#'
#' This function adds a new column to the `sample_info` slot of a mass_dataset object,
#' which contains the frequency of zero values for each sample according to the variables specified.
#'
#' @param object A mass_dataset object.
#' @param according_to_variables A character vector specifying the variable IDs to consider when
#'                              calculating the frequency of zero values. Default is "all", which considers all variables.
#'
#' @return A modified mass_dataset object with an updated `sample_info` slot.
#'
#'
#' @author Xiaotao Shen <shenxt1990@outlook.com>
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
#'   mutate_sample_zero_freq(object = object)
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
#'   mutate_sample_zero_freq(object = object2,
#'                 according_to_variables = variable_id)
#'
#' object3
#'
#' head(extract_sample_info(object3))

mutate_sample_zero_freq <-
  function(object,
           according_to_variables = "all") {
    check_object_class(object = object, class = "mass_dataset")
    
    variable_id <- get_variable_id(object)
    sample_id <- get_sample_id(object)
    
    if (any(according_to_variables == "all")) {
      according_to_variables <- variable_id
    } else{
      according_to_variables <-
        variable_id[variable_id %in% according_to_variables]
    }
    
    if (length(according_to_variables) == 0) {
      stop(
        "All the variables you provide in according_to_variables are not in the object.
           Please check."
      )
    }
    
    expression_data <-
      object@expression_data %>%
      as.data.frame()
    
    zero_freq <-
      expression_data[according_to_variables, ] %>%
      apply(2, function(x) {
        sum(x == 0) / length(according_to_variables)
      })
    
    new_column_name <-
      check_column_name(object@sample_info ,
                        column.name = "zero_freq")
    
    object@sample_info =
      cbind(object@sample_info,
            zero_freq = zero_freq) %>%
      as.data.frame()
    
    colnames(object@sample_info)[ncol(object@sample_info)] <-
      new_column_name
    
    rownames(object@sample_info) = NULL
    
    object <- update_sample_info(object)
    
    process_info <- object@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "mutate_sample_zero_freq()",
      parameter = list("according_to_variables" = according_to_variables),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "mutate_sample_zero_freq")) {
      process_info$mutate_sample_zero_freq <- parameter
    } else{
      process_info$mutate_sample_zero_freq <-
        c(process_info$mutate_sample_zero_freq,
          parameter)
    }
    
    object@process_info <- process_info
    
    return(object)
    
  }

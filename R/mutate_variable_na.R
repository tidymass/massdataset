#' Mutate Variable NA Count in mass_dataset Object
#'
#' This function adds a new column to the `variable_info` slot of a mass_dataset object,
#' which contains the count of NA (Not Available) values for each variable according to the samples specified.
#' @param object A mass_dataset object.
#' @param according_to_samples A character vector specifying the sample IDs to consider when
#'                            calculating the count of NA values. Default is "all", which considers all samples.
#'
#' @return A modified mass_dataset object with an updated `variable_info` slot.
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
#'     variable_info = variable_info,
#'   )
#'
#' object
#'
#' ##calculate NA number according to all the samples
#' object2 =
#'   mutate_variable_na_number(object = object)
#'
#' colnames(extract_variable_info(object))
#' colnames(extract_variable_info(object2))
#' object2@variable_info_note
#'
#' ##calculate NA number according to only QC samples
#' object3 <-
#'   mutate_variable_na_number(object = object2,
#'                 according_to_samples =
#'               get_sample_id(object)[extract_sample_info(object)$class == "QC"])
#'
#' object3
#'
#' colnames(extract_variable_info(object3))
#' object3@variable_info_note

mutate_variable_na_number <-
  function(object,
           according_to_samples = "all") {
    check_object_class(object = object, class = "mass_dataset")
    
    variable_id <- get_variable_id(object)
    sample_id <- get_sample_id(object)
    
    if (any(according_to_samples == "all")) {
      according_to_samples <- sample_id
    } else{
      according_to_samples <-
        sample_id[sample_id %in% according_to_samples]
    }
    
    if (length(according_to_samples) == 0) {
      stop(
        "All the samples you provide in according_to_samples are not in the object.
           Please check."
      )
    }
    
    expression_data <-
      object@expression_data %>%
      as.data.frame()
    
    na_number <-
      expression_data[, according_to_samples, drop = FALSE] %>%
      apply(1, function(x) {
        sum(is.na(x))
      })
    
    new_column_name <-
      check_column_name(df = object@variable_info,
                        column.name = "na_number")
    
    object@variable_info <-
      cbind(object@variable_info,
            na_number = na_number) %>%
      as.data.frame()
    
    colnames(object@variable_info)[ncol(object@variable_info)] <-
      new_column_name
    
    rownames(object@variable_info) <- NULL
    
    object <-
      update_variable_info(object = object)
    
    ###add new columns in variable_info_note
    process_info <- object@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "mutate_variable_na_number()",
      parameter = list("according_to_samples" = according_to_samples),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "mutate_variable_na_number")) {
      process_info$mutate_variable_na_number <- parameter
    } else{
      process_info$mutate_variable_na_number <-
        c(process_info$mutate_variable_na_number,
          parameter)
    }
    
    object@process_info <- process_info
    
    return(object)
    
  }




#' Mutate Variable NA Frequency in mass_dataset Object
#'
#' This function adds a new column to the `variable_info` slot of a mass_dataset object,
#' which contains the frequency of NA (Not Available) values for each variable according to the samples specified.
#'
#' @param object A mass_dataset object.
#' @param according_to_samples A character vector specifying the sample IDs to consider when
#'                            calculating the frequency of NA values. Default is "all", which considers all samples.
#'
#' @return A modified mass_dataset object with an updated `variable_info` slot.
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

mutate_variable_na_freq <-
  function(object,
           according_to_samples = "all") {
    check_object_class(object = object, class = "mass_dataset")
    
    variable_id <- get_variable_id(object)
    sample_id <- get_sample_id(object)
    
    if (any(according_to_samples == "all")) {
      according_to_samples <- sample_id
    } else{
      according_to_samples <-
        sample_id[sample_id %in% according_to_samples]
    }
    
    if (length(according_to_samples) == 0) {
      stop(
        "All the samples you provide in according_to_samples are not in the object.
           Please check."
      )
    }
    
    expression_data <-
      object@expression_data %>%
      as.data.frame()
    
    na_freq <-
      expression_data[, according_to_samples, drop = FALSE] %>%
      apply(1, function(x) {
        sum(is.na(x)) / length(according_to_samples)
      })
    
    new_column_name <-
      check_column_name(object@variable_info ,
                        column.name = "na_freq")
    
    object@variable_info <-
      cbind(object@variable_info,
            na_freq = na_freq) %>%
      as.data.frame()
    
    colnames(object@variable_info)[ncol(object@variable_info)] <-
      new_column_name
    
    rownames(object@variable_info) <- NULL
    
    ####variable_info_note
    # new_variable_info_note <-
    #   data.frame(name = setdiff(colnames(object@variable_info),
    #                             object@variable_info_note$name),
    #              meaning = setdiff(colnames(object@variable_info),
    #                                object@variable_info_note$name),
    #              check.names = FALSE)
    # object@variable_info_note <-
    #   rbind(object@variable_info_note,
    #         new_variable_info_note)
    # object@variable_info <- 
    # object@variable_info[, object@variable_info_note$name, drop = FALSE]
    #
    object <-
      update_variable_info(object = object)
    
    process_info <- object@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "mutate_variable_na_freq()",
      parameter = list("according_to_samples" = according_to_samples),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "mutate_variable_na_freq")) {
      process_info$mutate_variable_na_freq <- parameter
    } else{
      process_info$mutate_variable_na_freq <-
        c(process_info$mutate_variable_na_freq,
          parameter)
    }
    
    object@process_info <- process_info
    return(object)
  }

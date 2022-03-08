#' @title Add median intensity for each feature to variable_info
#' @description Add median intensity for each feature to variable_info
#' @docType methods
#' @rdname add_new_column-mass_dataset
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @param according_to_samples (required) What samples used to calculate median
#' intensity. Default is "all". If you
#' want to use only several samples, provide their names as a vector.
#' @param na.rm na.rm
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
#' ##calculate median intensity according to all the samples
#' object2 =
#'   mutate_median_intensity(object = object, na.rm = TRUE)
#'
#' object2
#'
#' head(extract_variable_info(object))
#' head(extract_variable_info(object2))
#'
#' ##calculate median intensity according to only QC samples
#' object3 =
#'   mutate_median_intensity(object = object2,
#'                 according_to_samples =
#'               get_sample_id(object)[extract_sample_info(object)$class == "QC"])
#'
#' object3
#'
#' head(extract_variable_info(object3))

mutate_median_intensity <-
  function(object,
           according_to_samples = "all",
           na.rm = TRUE) {
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
    
    median_intensity <-
      expression_data[, according_to_samples] %>%
      apply(1, function(x) {
        median(x, na.rm = na.rm)
      })
    
    new_column_name <-
      check_column_name(object@variable_info ,
                        column.name = "median_intensity")
    
    object@variable_info <-
      cbind(object@variable_info,
            median_intensity = median_intensity) %>%
      as.data.frame()
    
    colnames(object@variable_info)[ncol(object@variable_info)] <-
      new_column_name
    
    ####variable_info_note
    new_variable_info_note <-
      data.frame(
        name = setdiff(
          colnames(object@variable_info),
          object@variable_info_note$name
        ),
        meaning = setdiff(
          colnames(object@variable_info),
          object@variable_info_note$name
        ),
        check.names = FALSE
      )
    object@variable_info_note <-
      rbind(object@variable_info_note,
            new_variable_info_note)
    object@variable_info <-
      object@variable_info[, object@variable_info_note$name]
    
    process_info <- object@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "mutate_median_intensity()",
      parameter = list("according_to_samples" = according_to_samples),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "mutate_median_intensity")) {
      process_info$mutate_median_intensity <- parameter
    } else{
      process_info$mutate_median_intensity <-
        c(process_info$mutate_median_intensity,
          parameter)
    }
    
    object@process_info <- process_info
    
    return(object)
  }

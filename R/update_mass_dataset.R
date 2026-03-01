#' Update a mass_dataset Object
#'
#' This function updates the slots of a `mass_dataset` object to ensure consistency
#' among `sample_info`, `variable_info`, and `expression_data`. It also logs the
#' update process.
#'
#' @param object A `mass_dataset` object that you want to update.
#'
#' @return A `mass_dataset` object with updated slots and process information.
#'
#' @author Xiaotao Shen
#' \email{xiaotao.shen@outlook.com}
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
#' qc_object <- object[, extract_sample_info(object)$class == "QC"]
#' qc_object <- update_mass_dataset(object = qc_object)
#' dim(qc_object)
#'
#' feature_object <-
#'   object[extract_variable_info(object)$mz < 300, ]
#' feature_object <- update_mass_dataset(object = feature_object)
#' dim(feature_object)

update_mass_dataset <-
  function(object) {
    check_object_class(object = object, class = "mass_dataset")
    
    sample_info = object@sample_info
    sample_info_note = object@sample_info_note
    variable_info = object@variable_info
    variable_info_note = object@variable_info_note
    expression_data = object@expression_data
    
    intersect_sample_id = intersect(sample_info$sample_id,
                                    colnames(expression_data))
    
    intersect_variable_id =
      intersect(variable_info$variable_id,
                rownames(expression_data))
    
    expression_data =
      expression_data[intersect_variable_id, intersect_sample_id]
    
    sample_info =
      sample_info[match(intersect_sample_id, sample_info$sample_id),]
    
    variable_info =
      variable_info[match(intersect_variable_id, variable_info$variable_id),]
    
    object@sample_info = sample_info
    object@variable_info = variable_info
    object@expression_data = expression_data
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "update_mass_dataset()",
      parameter = list("no" = "no"),
      time = Sys.time()
    )
    
    process_info = object@process_info
    
    if (all(names(process_info) != "update_mass_dataset")) {
      process_info$update_mass_dataset = parameter
    } else{
      process_info$update_mass_dataset = c(process_info$update_mass_dataset,
                                           parameter)
    }
    
    object@process_info = process_info
    
    return(object)
  }


#' Update Variable Information in a mass_dataset Object
#'
#' This function updates the `variable_info` and `variable_info_note` slots of a `mass_dataset` object.
#' It ensures that the columns in `variable_info` are consistent with the names in `variable_info_note`.
#'
#' @param object A `mass_dataset` object whose `variable_info` and `variable_info_note` slots you want to update.
#'
#' @return A `mass_dataset` object with updated `variable_info` and `variable_info_note` slots.
#'
#' @author Xiaotao Shen
#' \email{xiaotao.shen@outlook.com}
#'
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object <- create_mass_dataset(
#'   expression_data = expression_data,
#'   sample_info = sample_info,
#'   variable_info = variable_info
#' )
#' updated_md <- update_variable_info(object)
#' head(extract_variable_info(updated_md))
#' head(extract_variable_info_note(updated_md))
#'
#' @export

update_variable_info <-
  function(object) {
    number1 <- ncol(object@variable_info)
    number2 <- nrow(object@variable_info_note)
    
    if (number1 > number2) {
      diff_name <-
        setdiff(colnames(object@variable_info),
                object@variable_info_note$name)
      new_variable_info_note <-
        data.frame(name = diff_name,
                   meaning = diff_name)
      variable_info_note <-
        rbind(object@variable_info_note,
              new_variable_info_note)
      object@variable_info_note <- variable_info_note
      object@variable_info <- 
        object@variable_info[, object@variable_info_note$name, drop = FALSE] 
      return(object)
    }
    
    if (number1 == number2) {
      if (all(sort(colnames(object@variable_info)) == 
              sort(object@variable_info_note$name))) {
        object@variable_info <- 
          object@variable_info[, object@variable_info_note$name, drop = FALSE] 
        return(object)
      }else{
        object@variable_info_note$name <- colnames(object@variable_info)
        return(object)
      }
    }
  }


#' Update Sample Information in a mass_dataset Object
#'
#' This function updates the `sample_info` and `sample_info_note` slots of a `mass_dataset` object.
#' It ensures that the columns in `sample_info` are consistent with the names in `sample_info_note`.
#'
#' @param object A `mass_dataset` object whose `sample_info` and `sample_info_note` slots you want to update.
#'
#' @return A `mass_dataset` object with updated `sample_info` and `sample_info_note` slots.
#'
#' @author Xiaotao Shen
#' \email{xiaotao.shen@outlook.com}
#'
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object <- create_mass_dataset(
#'   expression_data = expression_data,
#'   sample_info = sample_info,
#'   variable_info = variable_info
#' )
#' updated_md <- update_sample_info(object)
#' head(extract_sample_info(updated_md))
#' head(extract_sample_info_note(updated_md))
#'
#' @export

update_sample_info <-
  function(object) {
    number1 <- ncol(object@sample_info)
    number2 <- nrow(object@sample_info_note)
    
    if (number1 > number2) {
      diff_name <-
        setdiff(colnames(object@sample_info),
                object@sample_info_note$name)
      new_sample_info_note <-
        data.frame(name = diff_name,
                   meaning = diff_name)
      sample_info_note <-
        rbind(object@sample_info_note,
              new_sample_info_note)
      object@sample_info_note <- sample_info_note
      object@sample_info <- 
        object@sample_info[, object@sample_info_note$name, drop = FALSE] 
      return(object)
    }
    
    if (number1 == number2) {
      if (all(sort(colnames(object@sample_info)) == 
              sort(object@sample_info_note$name))) {
        object@sample_info <- 
          object@sample_info[, object@sample_info_note$name, drop = FALSE] 
        return(object)
      }else{
        object@sample_info_note$name <- colnames(object@sample_info)
        return(object)
      }
    }
  }

#' @title Update mass_dataset class object after give the one new item
#' @description Update mass_dataset class object after give the one new item
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @return A mass_dataset class object.
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
#' ####only remain QC samples
#' sample_info = extract_sample_info(object = object)
#'
#' sample_info =
#'   dplyr::filter(sample_info, class == "QC")
#'
#' object@sample_info = sample_info
#'
#' object = update_mass_dataset(object = object)
#' object
#'
#' ####only remain feature with mz < 300
#' variable_info = extract_variable_info(object = object)
#'
#' variable_info =
#'   dplyr::filter(variable_info, mz < 300)
#'
#' object@variable_info = variable_info
#'
#' object = update_mass_dataset(object = object)
#' object

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


#' @title update_variable_info
#' @description update_variable_info
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @return A mass_dataset class object.
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


#' @title update_sample_info
#' @description update_sample_info
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @return A mass_dataset class object.
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
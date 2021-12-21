#' @title Update mass_dataset class object after give the one new item
#' @description Update mass_dataset class object after give the one new item
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
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
#' ####only remain QC samples
#' library(tidyverse)
#' sample_info = extract_sample_info(object = object)
#'
#' sample_info =
#'   sample_info %>%
#'   dplyr::filter(class == "QC")
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
#'   variable_info %>%
#'   dplyr::filter(mz < 300)
#'
#' object@variable_info = variable_info
#'
#' object = update_mass_dataset(object = object)
#' object

update_mass_dataset =
  function(object) {
    if (class(object)[1] != "mass_dataset") {
      stop("only support mass_dataset class object.\n")
    }
    
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

#' Create ggplot2 Visualization for a mass_dataset Object
#'
#' This function generates a ggplot2 visualization for a mass_dataset object. 
#' You can choose the direction of the visualization (either "variable" or "sample"), 
#' specify the variable or sample of interest, and create various plots based on 
#' the chosen direction and data.
#'
#' @param object A mass_dataset object.
#' @param direction A character vector specifying the direction of the visualization. 
#'   It can be either "variable" or "sample". Default is "variable".
#' @param variable_index An integer specifying the index of the variable of interest.
#'   If not provided, you can use 'variable_id' to specify the variable by its ID.
#' @param variable_id A character string specifying the ID of the variable of interest.
#'   If provided, 'variable_index' will be ignored.
#' @param sample_index An integer specifying the index of the sample of interest.
#'   If not provided, you can use 'sample_id' to specify the sample by its ID.
#' @param sample_id A character string specifying the ID of the sample of interest.
#'   If provided, 'sample_index' will be ignored.
#'
#' @return A ggplot2 plot object.
#'
#' @author Xiaotao Shen <shenxt1990@outlook.com>
#' @import ggplot2
#' @export
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     variable_info = variable_info,
#'     sample_info = sample_info
#'   )
#' library(ggplot2)
#' ggplot_mass_dataset(object, direction = "variable", 
#'                     variable_index = 1) +
#'   geom_boxplot(aes(x = class, color = class)) +
#'   geom_jitter(aes(x = class, color = class))
#' 
#' object %>% 
#'   `+`(1) %>% 
#'   log(10) %>% 
#'   scale() %>% 
#' ggplot_mass_dataset(direction = "sample", 
#'                     sample_index = 2) +
#'   geom_boxplot(aes(x = 1)) +
#'   geom_jitter(aes(x = 1, color = mz))

 
ggplot_mass_dataset <-
  function(object, 
           direction = c("variable", "sample"),
           variable_index = 1,
           variable_id,
           sample_index = 1,
           sample_id) {
    check_object_class(object = object, class = "mass_dataset")
    direction <- match.arg(direction)
    expression_data = object@expression_data %>%
      as.data.frame()
    
    if(direction == "variable"){
      if(!missing(variable_id)){
        if(!variable_id %in% rownames(object)){
          stop(variable_id, " is not in variable_info$variable_id.")
        }else{
          variable_index <-
            match(variable_id, rownames(object))
        }
      }else{
        if(variable_index > nrow(object)){
          stop("variable_index should be a interger between 1 to ", 
               nrow(object), ", now is ", variable_index, ".")
        }
      }
      
      temp_data <- 
        data.frame(object@sample_info,
                   value = as.numeric(expression_data[variable_index,]),
                   check.names = FALSE)
    }
    
    if(direction == "sample"){
      if(!missing(sample_id)){
        if(!sample_id %in% colnames(object)){
          stop(sample_id, " is not in sample_info$sample_id.")
        }else{
          sample_index <-
            match(sample_id, colnames(object))
        }
      }else{
        if(sample_index > ncol(object)){
          stop("sample_index should be a interger between 1 to ", 
               ncol(object), ", now is ", sample_index, ".")
        }
      }
      
      temp_data <- 
        data.frame(object@variable_info,
                   value = as.numeric(expression_data[,sample_index]),
                   check.names = FALSE)
    }
    
    ggplot2::ggplot(data = temp_data, ggplot2::aes(y = value))
    
  }

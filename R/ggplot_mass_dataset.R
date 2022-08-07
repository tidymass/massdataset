#' @title Create a ggplot object from mass_dataset class
#' @description Create a ggplot object from mass_dataset class
#' @docType methods
#' @rdname graphics-mass_dataset
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @param direction variable or sample
#' @param variable_index variable index
#' @param variable_id variable ID
#' @param sample_index sample index
#' @param sample_id sample ID
#' @return A ggplot class
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

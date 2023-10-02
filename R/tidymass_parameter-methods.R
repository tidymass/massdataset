#' Translate tidymass_parameter Object to Data Frame
#'
#' This function takes a `tidymass_parameter` object and translates its slots into a data frame.
#' It is particularly useful for converting the parameter information into a more readable format.
#'
#' @param object A `tidymass_parameter` object that you want to translate.
#'
#' @return A data frame containing the package name, function name, parameters, and time of the `tidymass_parameter` object.
#'
#' @author Xiaotao Shen \email{shenxt1990@@outlook.com}
#'
#' @export
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info
#'   )
#' object
#' translate_tidymass_parameter(object@process_info[[1]])

translate_tidymass_parameter <-
  function(object) {
    check_object_class(object = object, class = "tidymass_parameter")
    
    if (length(object@parameter) == 0) {
      parameter <- "no:no"
    } else{
      parameter <-
        purrr::map2(names(object@parameter),
                    object@parameter, function(name, value) {
                      if (length(value) > 100000) {
                        value <- head(value, 100000)
                        value <-
                          paste(c(value, "..."), collapse = ',')
                      } else{
                        value <- paste(value, collapse = ',')
                      }
                      paste(name, value, sep = ":")
                    }) %>% unlist()
    }
    
    data.frame(
      pacakge_name = object@pacakge_name,
      function_name = object@function_name,
      parameter = parameter,
      time = object@time
    )
  }

#' @title translate_tidymass_parameters tidymass_parameters
#' @description translate_tidymass_parameters tidymass_parameters
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) tidymass_parameters class object.
#' @rdname tidymass_parameters-class
#' @return data.frame
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
#' translate_tidymass_parameters(object@process_info[[1]])

translate_tidymass_parameters =
  function(object) {
    data.frame(
      pacakge_name = object@pacakge_name,
      function_name = object@function_name,
      parameter = purrr::map2(names(object@parameter),
                              object@parameter, function(name, value) {
                                if (length(value) > 5) {
                                  value = head(value, 5)
                                  value = paste(c(value, "..."), collapse = ',')
                                } else{
                                  value = paste(value, collapse = ',')
                                }
                                paste(name, value, sep = ":")
                              }) %>% unlist(),
      time = object@time
    )
  }

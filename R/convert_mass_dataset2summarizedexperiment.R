#' Convert mass_dataset to SummarizedExperiment Object
#'
#' @author Xiaotao Shen
#' \email{xiaotao.shen@outlook.com}
#' @description This function converts a `mass_dataset` object to a `SummarizedExperiment` object.
#' It extracts the `sample_info`, `variable_info`, and `expression_data` from the `mass_dataset` object
#' and constructs a new `SummarizedExperiment` object.
#'
#' @param object A `mass_dataset` object containing `sample_info`, `variable_info`, and `expression_data`.
#'
#' @return A `SummarizedExperiment` object containing the same data as the input `mass_dataset` object.
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
#'
#' se <- convert_mass_dataset2summarizedexperiment(object)
#' se
#'
#' @details
#' The function checks if the input object is of class `mass_dataset`. If it is, it extracts the `sample_info`,
#' `variable_info`, and `expression_data` and uses these to create a new `SummarizedExperiment` object.
#'
#' @export

convert_mass_dataset2summarizedexperiment <-
  function(object) {
    if (!is(object = object, class2 = "mass_dataset")) {
      stop("Only support mass_dataset class.")
    }
    
    sample_info <-
      object@sample_info
    
    variable_info <-
      object@variable_info
    
    expression_data <-
      object@expression_data
    
    new_object <-
      SummarizedExperiment::SummarizedExperiment(
        assays = list(counts = as.matrix(expression_data)),
        rowData = S4Vectors::DataFrame(variable_info),
        colData = S4Vectors::DataFrame(sample_info)
      )
    
    return(new_object)
  }

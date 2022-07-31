#' @title convert_mass_dataset2summarizedexperiment
#' @description Convert mass_dataset class to SummarizedExperiment from
#' SummarizedExperiment package.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object mass_dataset class object.
#' @return A SummarizedExperiment file.
#' @importFrom dplyr filter rename
#' @importFrom stats time
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom S4Vectors DataFrame
#' @importClassesFrom SummarizedExperiment SummarizedExperiment SimpleAssays
#' @importClassesFrom S4Vectors DFrame
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

#' Convert mass_dataset to SummarizedExperiment Object
#'
#' @author Xiaotao Shen <shenxt1990@outlook.com>
#' @description This function converts a `mass_dataset` object to a `SummarizedExperiment` object.
#' It extracts the `sample_info`, `variable_info`, and `expression_data` from the `mass_dataset` object
#' and constructs a new `SummarizedExperiment` object.
#'
#' @param object A `mass_dataset` object containing `sample_info`, `variable_info`, and `expression_data`.
#'
#' @return A `SummarizedExperiment` object containing the same data as the input `mass_dataset` object.
#'
#' @examples
#' library(magrittr)
#' 
#' # Create expression data
#' expression_data_x <- data.frame(
#'   sample1 = c(1.1, 2.2, 3.3),
#'   sample2 = c(1.5, 2.5, 3.5),
#'   row.names = c("feature1", "feature2", "feature3")
#' )
#' 
#' # Sample info
#' sample_info_x <- data.frame(
#'   sample_id = c("sample1", "sample2"),
#'   batch = c(1, 1),
#'   age = c(30, 32),
#'   class = c("QC", "QC"),
#'   row.names = c("sample1", "sample2")
#' )
#' 
#' # Sample info note
#' sample_info_note_x <- data.frame(
#'   name = colnames(sample_info_x),
#'   meaning = c("Sample ID", "Batch ID", "Age", "Sample Class"),
#'   row.names = colnames(sample_info_x)
#' )
#' 
#' # Variable info
#' variable_info_x <- data.frame(
#'   variable_id = c("feature1", "feature2", "feature3"),
#'   row.names = c("feature1", "feature2", "feature3")
#' )
#' 
#' # Variable info note
#' variable_info_note_x <- data.frame(
#'   name = colnames(variable_info_x),
#'   meaning = "Feature ID",
#'   row.names = colnames(variable_info_x)
#' )
#' 
#' # Construct mass_dataset object
#' mass_dataset_object <- new(
#'   Class = "mass_dataset",
#'   expression_data = expression_data_x,
#'   sample_info = sample_info_x,
#'   sample_info_note = sample_info_note_x,
#'   variable_info = variable_info_x,
#'   variable_info_note = variable_info_note_x,
#'   ms2_data = list(),
#'   annotation_table = data.frame(),
#'   process_info = list(),
#'   other_files = list(),
#'   version = "1.0.0",
#'   activated = "expression_data"
#' )
#' 
#' # Convert to SummarizedExperiment
#' convert_mass_dataset2summarizedexperiment(mass_dataset_object)
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

#' Update Annotation Table in a mass_dataset Object
#'
#' This function updates the annotation table in a `mass_dataset` object by appending new annotation data and retaining only the top annotations based on the total score.
#'
#' @param object A `mass_dataset` object containing the existing annotation table.
#' @param annotation_table A data frame containing new annotation data to be added to the `mass_dataset` object. The table should contain specific columns as detailed below.
#'
#' @return The function returns the updated `mass_dataset` object with the appended and processed annotation table.
#'
#' @details
#' This function allows you to update or append new annotations to the existing annotation table in a `mass_dataset` object. The function ensures that the new annotations are properly arranged based on the `Total.score`, and it retains only the top annotations for each `variable_id` based on the specified `candidate.num`.
#'
#' The `annotation_table` must contain the following columns:
#' \describe{
#'   \item{variable_id}{The unique identifier for each variable.}
#'   \item{ms2_files_id}{The ID of the MS2 file associated with each annotation.}
#'   \item{ms2_spectrum_id}{The ID of the MS2 spectrum associated with each annotation.}
#'   \item{Compound.name}{The name of the annotated compound.}
#'   \item{CAS.ID}{The CAS ID of the compound.}
#'   \item{HMDB.ID}{The HMDB ID of the compound.}
#'   \item{KEGG.ID}{The KEGG ID of the compound.}
#'   \item{Lab.ID}{The lab identifier for the compound.}
#'   \item{Adduct}{The adduct form of the compound.}
#'   \item{mz.error}{The error in m/z matching.}
#'   \item{mz.match.score}{The score for the m/z match.}
#'   \item{RT.error}{The error in retention time matching.}
#'   \item{RT.match.score}{The score for the retention time match.}
#'   \item{CE}{Collision energy used in MS2 matching.}
#'   \item{SS}{Spectral similarity score for MS2 matching.}
#'   \item{Total.score}{The total score for the annotation.}
#'   \item{Database}{The database used for annotation.}
#'   \item{Level}{The confidence level of the annotation.}
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage
#' updated_object <- mutate_annotation_table(
#'   object = my_dataset,
#'   annotation_table = new_annotation_data
#' )
#' }
#'
#'
#' @export


mutate_annotation_table <-
  function(object, annotation_table) {
    if (missing(annotation_table)) {
      stop("annotation_table is missing.")
    }
    
    if (is.null(annotation_table)) {
      return(object)
    }
    
    if (nrow(annotation_table) == 0) {
      return(object)
    }
    
    check_annotation_table(annotation_table)
    
    if (missing(object)) {
      stop("object is missing.")
    }
    
    if (!is(object, "mass_dataset")) {
      stop("object must be a mass_dataset object.")
    }
    
    ###processing information
    process_info <- object@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "mutate_annotation_table()",
      parameter = list(),
      time = Sys.time()
    )
    
    
    if (all(names(process_info) != "mutate_annotation_table")) {
      process_info$mutate_annotation_table <- parameter
    } else{
      process_info$mutate_annotation_table <-
        c(process_info$mutate_annotation_table, parameter)
    }
    
    object@process_info <- process_info
    
    annotation_table <-
      annotation_table[, c(
        "variable_id",
        "ms2_files_id",
        "ms2_spectrum_id",
        "Compound.name",
        "CAS.ID",
        "HMDB.ID",
        "KEGG.ID",
        "Lab.ID",
        "Adduct",
        "mz.error",
        "mz.match.score",
        "RT.error",
        "RT.match.score",
        "CE",
        "SS",
        "Total.score",
        "Database",
        "Level"
      )]
    
    if (nrow(object@annotation_table) == 0) {
      object@annotation_table <- annotation_table
    } else{
      object@annotation_table <-
        rbind(object@annotation_table, annotation_table) %>%
        dplyr::arrange(variable_id, Level, dplyr::desc(Total.score))
      
      ###only remain top annotations
      object@annotation_table <-
        object@annotation_table %>%
        dplyr::group_by(variable_id) %>%
        dplyr::slice_head(n = candidate.num) %>%
        dplyr::ungroup() %>%
        dplyr::distinct(.keep_all = TRUE)
    }
    
    return(object)
    
  }

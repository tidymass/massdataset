#' Validate Annotation Table Format
#'
#' This function checks if an annotation table meets the required format for metabolite annotation. It verifies the presence of necessary columns, data types, and ensures that no critical values are missing (NA).
#'
#' @param annotation_table A data frame containing the annotation data to be validated.
#'
#' @return
#' The function returns a message indicating that the `annotation_table` meets the required format, or it stops with an error message if any checks fail.
#'
#' @examples
#' \dontrun{
#' # Assuming `my_annotation_table` is the table to validate:
#' check_annotation_table(my_annotation_table)
#' }
#'
#'
#' @export


check_annotation_table <-
  function(annotation_table) {
    # Check if annotation_table is provided
    if (missing(annotation_table)) {
      stop("The annotation_table is missing.")
    }
    
    # Check if annotation_table is a data frame
    if (!is.data.frame(annotation_table)) {
      stop("The annotation_table must be a data frame.")
    }
    
    # Define required columns
    required_columns <- c(
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
    )
    
    # Check if all required columns are present
    if (!all(required_columns %in% colnames(annotation_table))) {
      missing_cols <- setdiff(required_columns, colnames(annotation_table))
      stop(paste(
        "The following required columns are missing:",
        paste(missing_cols, collapse = ", ")
      ))
    }
    
    # Check if there are any missing (NA) values in the required columns
    if (any(is.na(annotation_table[c("variable_id", "Compound.name", "Database", "Level")]))) {
      stop("The annotation_table contains missing values (NA) in required columns.")
    }
    
    # Check that specific columns have the correct data types
    if (!is.character(annotation_table$variable_id)) {
      stop("The 'variable_id' column must be of type character.")
    }
    
    if (!is.character(annotation_table$Compound.name)) {
      stop("The 'Compound.name' column must be of type character.")
    }
    
    
    # Check if numeric columns are indeed numeric
    numeric_columns <- c("Total.score")
    
    for (col in numeric_columns) {
      if (!is.numeric(annotation_table[[col]])) {
        stop(paste("The", col, "column must be of type numeric."))
      }
    }
    #
    # # If all checks pass
    # message("The annotation_table meets the required format.")
  }

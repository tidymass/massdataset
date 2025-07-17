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
#' # Create a valid annotation table
#' annotation_table <- data.frame(
#'   variable_id = c("feature1", "feature2"),
#'   ms2_files_id = c("file1", "file2"),
#'   ms2_spectrum_id = c("spec1", "spec2"),
#'   Compound.name = c("Glucose", "Fructose"),
#'   CAS.ID = c("50-99-7", "57-48-7"),
#'   HMDB.ID = c("HMDB0000122", "HMDB0000660"),
#'   KEGG.ID = c("C00031", "C00095"),
#'   Lab.ID = c("Lab001", "Lab002"),
#'   Adduct = c("[M+H]+", "[M+Na]+"),
#'   mz.error = c(0.001, 0.002),
#'   mz.match.score = c(0.95, 0.96),
#'   RT.error = c(0.1, 0.2),
#'   RT.match.score = c(0.9, 0.92),
#'   CE = c(20, 30),
#'   SS = c("MS2_001", "MS2_002"),
#'   Total.score = c(0.85, 0.88),
#'   Database = c("HMDB", "KEGG"),
#'   Level = c("Level 1", "Level 2"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Validate the table
#' check_annotation_table(annotation_table)
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

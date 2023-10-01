#' Export mass_dataset Object for MetDNA Analysis
#'
#' @description
#' This function exports a \code{\link[massdataset]{mass_dataset}} object into a format suitable for MetDNA analysis.
#' It generates a Peak_Table.csv, sample_info.csv, and MS2 data in MGF format.
#'
#' @param object An object of class \code{mass_dataset}.
#' @param path A character string indicating the directory where the exported files will be saved. Default is the current directory.
#'
#' @return
#' This function writes the Peak_Table.csv, sample_info.csv, and MS2 data files to the specified directory.
#'
#' @seealso
#' \code{\link[massdataset]{mass_dataset}} for the class that this function exports.
#' \code{\link[massdataset]{export_ms2_data}} for exporting MS2 data.
#'
#' @examples
#' \dontrun{
#' # Create a new mass_dataset object
#' new_data <- create_mass_dataset(
#'   expression_data = data.frame(),
#'   sample_info = data.frame(),
#'   variable_info = data.frame(),
#'   sample_info_note = data.frame(),
#'   variable_info_note = data.frame()
#' )
#'
#' # Export the data for MetDNA analysis
#' export_mass_dataset4metdna(new_data, path = "MetDNA_data/")
#' }
#'
#' @export
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}


export_mass_dataset4metdna <-
  function(object, path = ".") {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    object <-
      object %>%
      activate_mass_dataset(what = "sample_info") %>%
      filter(class == "Subject")
    
    variable_info <-
      extract_variable_info(object)
    
    sample_info <-
      extract_sample_info(object)
    
    expression_data <-
      extract_expression_data(object)
    
    Peak_Table <-
      data.frame(
        name = variable_info$variable_id,
        mz = variable_info$mz,
        rt = variable_info$rt,
        expression_data
      )
    
    sample_info <-
      sample_info %>%
      dplyr::select(sample_id, group) %>%
      dplyr::rename(sample.name = sample_id)
    
    readr::write_csv(Peak_Table, file.path(path, "Peak_Table.csv"))
    readr::write_csv(sample_info, file.path(path, "sample_info.csv"))
    
    export_ms2_data(object = object,
                    file_type = "mgf",
                    path = path)
  }
#' @title Export mass_dataset Object to Files
#' 
#' @description
#' Exports the contents of a \code{mass_dataset} object to files. The files can be in CSV or XLSX format.
#' Additionally, MS2 data can be exported in MSP or MGF format.
#'
#' @param object A \code{mass_dataset} object to be exported.
#' @param file_type The type of file to export the data to. Options are "csv" or "xlsx". Default is "csv".
#' @param ms2_file_type The type of file to export the MS2 data to. Options are "msp" or "mgf". Default is "msp".
#' @param path The directory where the exported files will be saved. Default is the current directory.
#' 
#' @return No return value. The function writes files to disk.
#' 
#' @examples
#' \dontrun{
#' # Assuming 'data' is a mass_dataset object
#' export_mass_dataset(data, file_type = "csv", path = "output/")
#' }
#' 
#' @export
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}

export_mass_dataset <-
  function(object,
           file_type = c("csv", "xlsx"),
           ms2_file_type = c("msp", "mgf"),
           path = ".") {
    check_object_class(object = object, class = "mass_dataset")
    file_type <- match.arg(file_type)
    ms2_file_type <- match.arg(ms2_file_type)
    dir.create(path, showWarnings = FALSE)
    # if (class(object)[1] != "mass_dataset") {
    if (!is(object, class2 = "mass_dataset")) {
      stop("Only support mass_dataset class object.\n")
    }
    
    if (file_type == "csv") {
      readr::write_csv(object@expression_data,
                       file = file.path(path, "expression_data.csv"))
      readr::write_csv(object@sample_info,
                       file = file.path(path, "sample_info.csv"))
      readr::write_csv(object@variable_info,
                       file = file.path(path, "variable_info.csv"))
      readr::write_csv(object@sample_info_note,
                       file = file.path(path, "sample_info_note.csv"))
      readr::write_csv(object@variable_info_note,
                       file = file.path(path, "variable_info_note.csv"))
    } else{
      openxlsx::write.xlsx(
        object@expression_data,
        file = file.path(path, "expression_data.xlsx"),
        asTable = TRUE,
        overwrite = TRUE
      )
      openxlsx::write.xlsx(
        object@sample_info,
        file = file.path(path, "sample_info.xlsx"),
        asTable = TRUE,
        overwrite = TRUE
      )
      openxlsx::write.xlsx(
        object@variable_info,
        file = file.path(path, "variable_info.xlsx"),
        asTable = TRUE,
        overwrite = TRUE
      )
      if (nrow(object@sample_info_note) > 0) {
        openxlsx::write.xlsx(
          object@sample_info_note,
          file = file.path(path, "sample_info_note.xlsx"),
          asTable = TRUE,
          overwrite = TRUE
        )
      }
      
      if (nrow(object@variable_info_note) > 0) {
        openxlsx::write.xlsx(
          object@variable_info_note,
          file = file.path(path, "variable_info_note.xlsx"),
          asTable = TRUE,
          overwrite = TRUE
        )
      }
    }
    ###ms2_data
    if (length(object@ms2_data) > 0) {
      if (is(object@ms2_data[[1]], class2 = "ms2_data")) {
        export_ms2_data(
          object = object,
          file_type = ms2_file_type,
          path = path
        )
      }
    }
  }


#' @title Export MS2 Data from mass_dataset Object
#' 
#' @description
#' Exports the MS2 data contained in a \code{mass_dataset} object to files. 
#' The files can be in MGF or MSP format.
#'
#' @param object A \code{mass_dataset} object containing MS2 data to be exported.
#' @param file_type The type of file to export the MS2 data to. Options are "mgf" or "msp". Default is "mgf".
#' @param path The directory where the exported MS2 data files will be saved. Default is the current directory.
#' 
#' @return No return value. The function writes MS2 data files to disk. 
#' Returns NULL and a warning if no MS2 data is present in the object.
#' 
#' @examples
#' \dontrun{
#' # Assuming 'data' is a mass_dataset object with MS2 data
#' export_ms2_data(data, file_type = "mgf", path = "output/")
#' }
#'
#' 
#' @export
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}

export_ms2_data <-
  function(object,
           file_type = c("mgf", "msp"),
           path = ".") {
    file_type = match.arg(file_type)
    dir.create(path, showWarnings = FALSE)
    # if (class(object)[1] != "mass_dataset") {
    if (!is(object, class2 = "mass_dataset")) {
      stop("Only support mass_dataset class object.\n")
    }
    
    if (length(object@ms2_data) == 0) {
      warning("No MS2 data.\n")
      return(NULL)
    }
    
    message(crayon::yellow("Write MS2 data..."))
    
    purrr::walk2(
      .x = names(object@ms2_data),
      .y = object@ms2_data,
      .f = function(temp_name, temp_data) {
        message(crayon::yellow(temp_name))
        file_name =
          paste("ms2_data_",
                match(temp_name, names(object@ms2_data)),
                sep = "")
        unlink(
          x = file.path(path, file_name),
          recursive = TRUE,
          force = TRUE
        )
        
        write_ms2_data(
          object = temp_data,
          file_type = file_type,
          file_name = file_name,
          path = path
        )
        
      }
    )
    message(crayon::green("Done."))
  }

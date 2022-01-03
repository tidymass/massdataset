#' @title export_mass_dataset
#' @description Export mass_dataset class object to csv/xlsx files
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @param file_type (required) csv or xlsx
#' @param ms2_file_type (required) msp or mgf
#' @param path (required) work directory.
#' @return csv or xlsx files.
#' @export

export_mass_dataset =
  function(object,
           file_type = c("csv", "xlsx"),
           ms2_file_type = c("csv", "xlsx"),
           path = "."
           ) {
    check_object_class(object = object, class = "mass_dataset")
    file_type = match.arg(file_type)
    ms2_file_type = match.arg(ms2_file_type)
    dir.create(path, showWarnings = FALSE)
    if (class(object)[1] != "mass_dataset") {
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
      
      ###ms2_data
      export_ms2_data(object = object,
                      file_type = ms2_file_type,
                      path = path)
    }
  }


#' @title export_ms2_data
#' @description Export mass_dataset's ms2_data to mgf/msp
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @param file_type (required) mgf, msp
#' @param path (required) work directory.
#' @return mgf, msp files
#' @export

export_ms2_data =
  function(object,
           file_type = c("mgf", "msp"),
           path = ".") {
    check_object_class(object = object, class = "ms2_data")
    file_type = match.arg(file_type)
    dir.create(path, showWarnings = FALSE)
    if (class(object)[1] != "mass_dataset") {
      stop("Only support mass_dataset class object.\n")
    }
    
    if (length(object@ms2_data) == 0) {
      warning("No MS2 data.\n")
      return(NULL)
    }
    
    cat(crayon::yellow("Write MS2 data...\n"))
    
    purrr::walk2(
      .x = names(object@ms2_data),
      .y = object@ms2_data,
      .f = function(temp_name, temp_data) {
        cat(crayon::yellow(temp_name, "\n"))
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
    cat(crayon::green("Done.\n"))
  }

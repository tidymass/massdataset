#' @title Export mass_dataset class object to csv/xlsx files
#' @description Export mass_dataset class object to csv/xlsx files
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @param file_type (required) csv or xlsx
#' @param path work directory.
#' @return csv or xlsx files.
#' @export

export_mass_dataset =
  function(object,
           file_type = c("csv", "xlsx"),
           path = ".") {
    file_type = match.arg(file_type)
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
    }
  }

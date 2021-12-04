#' @title Export tidymass class object to csv/xlsx files
#' @description Export tidymass class object to csv/xlsx files
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) tidymass class object.
#' @param file_type (required) csv or xlsx
#' @param path work directory.
#' @export

export_tidymass_class =
  function(object,
           file_type = c("csv", "xlsx"),
           path = ".") {
    file_type = match.arg(file_type)
    if(class(object)[1] != "tidymass"){
      stop("Only support tidymass class object.\n")
    }
    
    if(file_type == "csv"){
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
    }else{
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
      if(nrow(object@sample_info_note) > 0){
        openxlsx::write.xlsx(
          object@sample_info_note,
          file = file.path(path, "sample_info_note.xlsx"),
          asTable = TRUE,
          overwrite = TRUE
        )  
      }
      
      if(nrow(object@variable_info_note) > 0){
        openxlsx::write.xlsx(
          object@variable_info_note,
          file = file.path(path, "variable_info_note.xlsx"),
          asTable = TRUE,
          overwrite = TRUE
        )  
      }
    }
  }

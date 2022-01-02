#' @title report_parameters
#' @description report_parameters
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) tidymass_parameters class object.
#' @param path working directory
#' @return html report
#' @export
#' @examples
#' \dontrun{
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info
#'   )
#' object
#'
#' library(tidyverse)
#'
#' object =
#' object %>%
#'   activate_mass_dataset(what = "expression_data") %>%
#'   filter(!is.na(QC_1))
#'
#' object =
#'   object %>%
#'   activate_mass_dataset(what = "expression_data") %>%
#'   filter(!is.na(QC_2))
#'
#'
#' report_parameters(object = object, path = "demo_data")
#' }

report_parameters =
  function(object,
           path = ".") {
    dir.create(path, showWarnings = FALSE)
    
    ###get parameters
    parameters =
      lapply(object@process_info, function(x) {
        if (class(x)[1] == "tidymass_parameter") {
          translate_tidymass_parameters(object = x)
        } else{
          lapply(x, function(y) {
            translate_tidymass_parameters(object = y)
          }) %>%
            dplyr::bind_rows()
        }
        
      }) %>%
      dplyr::bind_rows()
    
    save(parameters, file = file.path(path, "parameters.rda"))
    
    rmarkdown::draft(
      file = path,
      template = "tidymass_parameters",
      package = "massdataset",
      create_dir = TRUE,
      edit = FALSE
    )
    
    
    rmarkdown::render(
      file.path(path, "tidymass_parameters.template.Rmd"),
      rmarkdown::html_document()
    )
    file.rename(
      from = file.path(path, "tidymass_parameters.template.html"),
      to = file.path(path, "massqc_report.html")
    )
    
    file = dir(path)
    remove_file = grep("png|Rmd|parameters|rda", file, value = TRUE)
    unlink(
      x = file.path(path, remove_file),
      recursive = TRUE,
      force = TRUE
    )
  }

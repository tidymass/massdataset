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
#' object =
#' object %>%
#'   mutate_mean_intensity()
#' 
#' object =
#'   object %>%
#'   mutate_median_intensity() %>% 
#'   mutate_rsd()
#'
#' report_parameters(object = object, path = "demo_data")
#' }

report_parameters =
  function(object,
           path = ".") {
    options(warn = -1)
    dir.create(path, showWarnings = FALSE)
    
    ###path
    if (length(grep("Parameter_report", dir(path))) > 0) {
      output_path = file.path(path, paste('Parameter_report', length(grep(
        "Parameter_report", dir(path)
      )) + 1, sep = "_"))
    } else{
      output_path = file.path(path, "Parameter_report")
    }
    
    rmarkdown::draft(
      file = output_path,
      template = "tidymass_parameters",
      package = "massdataset",
      create_dir = TRUE,
      edit = FALSE
    )
    
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
    
    save(parameters, file = file.path(output_path, "parameters.rda"))
    
    rmarkdown::render(
      file.path(output_path, "tidymass_parameters.template.Rmd"),
      rmarkdown::html_document()
    )
    
    file.rename(
      from = file.path(output_path, "tidymass_parameters.template.html"),
      to = file.path(output_path, "parameter_report.html")
    )
    
    file = dir(output_path)
    remove_file = grep("png|Rmd|parameters|rda", file, value = TRUE)
    unlink(
      x = file.path(output_path, remove_file),
      recursive = TRUE,
      force = TRUE
    )
  
    file.copy(from = file.path(output_path, "parameter_report.html"), 
              to = file.path(path, "parameter_report.html"), overwrite = TRUE)
    unlink(file.path(output_path), recursive = TRUE, force = TRUE)
    cat(crayon::yellow("Done.\n"))
    options(warn = 0)
  }

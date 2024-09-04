#' Generate Parameter Report for mass_dataset Object
#'
#' This function generates an HTML report summarizing the parameters used in the processing of a `mass_dataset` object. The report is saved in a specified directory.
#'
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object A `mass_dataset` object for which the parameter report is to be generated.
#' @param path The directory where the parameter report will be saved. Default is the current working directory.
#'
#' @return An HTML report summarizing the parameters used in the processing of the `mass_dataset` object. The report is saved in the specified directory.
#'
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

report_parameters <-
  function(object,
           path = ".") {
    check_object_class(object = object, class = "mass_dataset")
    
    options(warn = -1)
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    
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
    parameters <- 
      object@process_info %>% 
      lapply(function(x){
        if(length(x) == 1){
          parse_tidymass_parameter(object = x)
        }else{
          x %>% 
            lapply(function(y){
              parse_tidymass_parameter(object = y)
            }) %>% 
            dplyr::bind_rows()
        }
      }) %>% 
      dplyr::bind_rows() %>% 
      dplyr::arrange(time)
    
    save(parameters, file = file.path(output_path, "parameters.rda"))
    
    rmarkdown::render(
      file.path(output_path, "tidymass_parameters.template.Rmd"),
      rmarkdown::html_document()
    )
    
    file.rename(
      from = file.path(output_path, "tidymass_parameters.template.html"),
      to = file.path(output_path, "parameter_report.html")
    )
    
    file <- dir(output_path)
    remove_file <- grep("png|Rmd|parameters|rda", file, value = TRUE)
    unlink(
      x = file.path(output_path, remove_file),
      recursive = TRUE,
      force = TRUE
    )
    
    file.copy(
      from = file.path(output_path, "parameter_report.html"),
      to = file.path(path, "parameter_report.html"),
      overwrite = TRUE
    )
    unlink(file.path(output_path),
           recursive = TRUE,
           force = TRUE)
    message(crayon::yellow("Done."))
    options(warn = 0)
  }


#' Parse tidymass_parameter Object to Data Frame
#'
#' This function takes a `tidymass_parameter` object and converts it into a data frame for easier manipulation and reporting.
#'
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object A `tidymass_parameter` object to be parsed.
#'
#' @return A data frame containing the package name, function name, parameters, and the time when the function was called.
#'
#' @export
#' @examples
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
#' parse_tidymass_parameter(object@process_info$create_mass_dataset)


parse_tidymass_parameter <-
  function(object) {
    if (!is(object, class2 = "tidymass_parameter")) {
      stop("only support tidymass_parameter class.\n")
    }
    
    if (is.null(names(object@parameter))) {
      names(object@parameter) = paste("parameter",
                                      seq_along(object@parameter),
                                      sep = "_")
    }
    
    result <-
      data.frame(
        pacakge_name = object@pacakge_name,
        function_name = object@function_name,
        parameter = purrr::map2(names(object@parameter),
                                object@parameter,
                                function(name, value) {
                                  if (length(value) > 5) {
                                    value = head(value, 5)
                                    value = paste(c(value, "..."), collapse = ',')
                                  } else{
                                    value = paste(value, collapse = ',')
                                  }
                                  paste(name, value, sep = ":")
                                }) %>% unlist(),
        time = object@time,
        check.names = FALSE
      )
    
    return(result)
    
  }

#' @title export_mass_dataset4metdna
#' @description Export mass_dataset to MetDNA require data format.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object mass_dataset class object.
#' @param path The path to save the mzTab-m file.
#' @return csv files
#' @importFrom magrittr %>%
#' @importFrom dplyr filter rename
#' @importFrom stats time
#' @export

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
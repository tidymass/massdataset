#' @title convert_msdial2mass_dataset
#' @description Convert MS-DIAL feature table to mass_dataset class.
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param x Feature table from MS-DIAL.
#' @return A mass_dataset-class object.
#' @importFrom dplyr filter rename
#' @importFrom stats time
#' @export
#' @examples
#' 
#' data("msdial_table")
#' object =
#'   convert_msdial2mass_dataset(x = msdial_table)
#' object

convert_msdial2mass_dataset <-
  function(x) {
    sample_info_idx <- 
      which(x$V1 == "")
    
    sample_info_idx <-
      c(sample_info_idx, max(sample_info_idx) + 1)
    
    sample_info <- 
      x[sample_info_idx,] %>% 
      t() %>% 
        as.data.frame()
    
    expression_data_idx <-
      which(sample_info$`1` != "" & !is.na(sample_info$`1`))[-1]
    
    expression_data <- 
      x[-sample_info_idx[-length(sample_info_idx)],expression_data_idx]
    
    colnames(expression_data) <-
      as.character(expression_data[1,])
    
    expression_data <- 
      expression_data[-1,]
    
    variable_info <- 
      x[-sample_info_idx[-length(sample_info_idx)],-expression_data_idx]
    
    colnames(variable_info) <- as.character(variable_info[1,])
    variable_info <- 
      variable_info[-1,]
    
    variable_info <-
      variable_info %>% 
      dplyr::select(-which(colnames(variable_info) == "1"))
    
    dim(expression_data)
    dim(variable_info)
    
    sample_info <-
      sample_info %>% 
      dplyr::filter(!is.na(`1`)) %>% 
      dplyr::filter(`1` != "")
    
    colnames(sample_info) <-
      as.character(sample_info[1,])
    
    sample_info <-
      sample_info[-1,]
    
    rownames(sample_info) <- NULL
    rownames(variable_info) <- NULL
    
    colnames(sample_info)[ncol(sample_info)] <- 
      "sample_id"
    
    sample_info <- 
    sample_info %>% 
      dplyr::select(sample_id, dplyr::everything()) %>% 
      dplyr::rename(class = Class, 
                    injection.order = "Injection order",
                    batch = "Batch ID")
    
    colnames(expression_data) <-
      sample_info$sample_id
    
    variable_info <- 
      variable_info %>% 
      dplyr::rename(variable_id = "Alignment ID",
                    mz = "Average Mz",
                    rt = "Average Rt(min)") %>% 
      dplyr::select(variable_id, mz, rt, dplyr::everything())
    
    rownames(expression_data) <-
      variable_info$variable_id

    variable_info <- 
    variable_info %>% 
      dplyr::mutate(mz = as.numeric(mz),
                    rt = as.numeric(rt))
    
      variable_info$rt <-
        variable_info$rt * 60

    
      sample_info_note <-
        data.frame(name = colnames(sample_info),
                   meaning = colnames(sample_info),
                   check.names = FALSE)
    
      variable_info_note <-
        data.frame(
          name = colnames(variable_info),
          meaning = colnames(variable_info),
          check.names = FALSE
        )
    
    check_result <-
      check_mass_dataset(
        expression_data = expression_data,
        sample_info = sample_info,
        variable_info = variable_info,
        sample_info_note = sample_info_note,
        variable_info_note = variable_info_note
      )
    
    if (stringr::str_detect(check_result, "error")) {
      stop(check_result)
    }
    
    process_info = list()
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "convert_msdial2mass_dataset()",
      parameter = list("no" = "no"),
      time = Sys.time()
    )
    
    process_info$create_mass_dataset = parameter
    
    object <- new(
      Class = "mass_dataset",
      expression_data = expression_data,
      ms2_data = list(),
      annotation_table = data.frame(),
      sample_info = sample_info,
      variable_info = variable_info,
      sample_info_note = sample_info_note,
      variable_info_note = variable_info_note,
      process_info = process_info,
      version = as.character(utils::packageVersion(pkg = "massdataset"))
    )
    invisible(object)
  }

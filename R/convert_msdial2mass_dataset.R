#' Convert MS-Dial Data to mass_dataset Object
#'
#' @author Xiaotao Shen <shenxt1990@outlook.com>
#' @description This function converts data from MS-Dial into a `mass_dataset` object.
#' It reads the data either from a given data frame or from a file and processes it to create a `mass_dataset` object.
#'
#' @param x A data frame containing MS-Dial data. If missing, the function will read from a file.
#' @param path The directory where the file is located. Default is the current directory.
#' @param file_name The name of the file to read if `x` is missing.
#'
#' @return A `mass_dataset` object containing the processed MS-Dial data.
#'
#' @details
#' The function reads MS-Dial data and processes it to create a `mass_dataset` object.
#' It extracts sample information, variable information, and expression data.
#' It also performs checks to ensure the data is correctly formatted.
#'
#' @export
#' @examples
#' ##if you want to read the msdital table,
#' ##use this function
#' ## table <- read.table("msdial_table.csv", sep = ",")
#' data("msdial_table")
#' object =
#'   convert_msdial2mass_dataset(x = msdial_table)
#' object

convert_msdial2mass_dataset <-
  function(x,
           path = ".",
           file_name) {
    if (missing(x)) {
      if (missing(file_name)) {
        stop("Provide x or file_name")
      }
      
      # x <-
      #   read.delim(file.path(path, file_name),
      #              header = FALSE)
      x <- utils::read.table(file.path(path, file_name), sep = ",")
      
    }
    
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
      x[-sample_info_idx[-length(sample_info_idx)], expression_data_idx]
    
    colnames(expression_data) <-
      as.character(expression_data[1,])
    
    expression_data <-
      expression_data[-1,]
    
    variable_info <-
      x[-sample_info_idx[-length(sample_info_idx)], -expression_data_idx]
    
    colnames(variable_info) <- as.character(variable_info[1,])
    variable_info <-
      variable_info[-1,]
    
    remove_idx <-
      which(colnames(variable_info) %in% c(1:100))
    
    if (length(remove_idx) > 0) {
      variable_info <-
        variable_info[, -remove_idx]
    }
    
    if (sum(colnames(variable_info) == "1") != 0) {
      variable_info <-
        variable_info %>%
        dplyr::select(-which(colnames(variable_info) == "1"))
    }
    
    # dim(expression_data)
    # dim(variable_info)
    
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
      dplyr::select(sample_id, dplyr::everything())
    
    if (any(colnames(sample_info) == "Class")) {
      sample_info <-
        sample_info %>%
        dplyr::rename(class = Class)
    }
    
    if (any(colnames(sample_info) == "Injection order")) {
      sample_info <-
        sample_info %>%
        dplyr::rename(injection.order = "Injection order")
    }
    
    if (any(colnames(sample_info) == "Batch ID")) {
      sample_info <-
        sample_info %>%
        dplyr::rename(batch = "Batch ID")
    }
    
    colnames(expression_data) <-
      sample_info$sample_id
    
    colnames(variable_info) <-
      masstools::name_duplicated(colnames(variable_info))
    
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
      data.frame(
        name = colnames(sample_info),
        meaning = colnames(sample_info),
        check.names = FALSE
      )
    
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

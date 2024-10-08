#' Convert MZmine Data to mass_dataset Object
#'
#' @author Xiaotao Shen <shenxt1990@outlook.com>
#' @description This function converts data from MZmine into a `mass_dataset` object.
#' It processes the data to create a `mass_dataset` object containing expression data, sample information, and variable information.
#'
#' @param x A data frame containing MZmine data.
#' @param rt_unit The unit for retention time, either "minute" or "second". Default is "minute".
#'
#' @return A `mass_dataset` object containing the processed MZmine data.
#'
#'
#' @details
#' The function reads MZmine data and processes it to create a `mass_dataset` object.
#' It extracts sample information, variable information, and expression data.
#' It also performs checks to ensure the data is correctly formatted.
#'
#' @export
#' @examples
#' data("mzmine_table")
#' head(mzmine_table[,1:3])
#' object =
#'   convet_mzmine2mass_dataset(x = mzmine_table)
#' object
convet_mzmine2mass_dataset <-
  function(x,
           rt_unit = c("minute" , "second")) {
    rt_unit <- match.arg(rt_unit)
    variable_info <-
      x %>%
      dplyr::select(`row ID`:`row retention time`)
    
    expression_data <-
      x %>%
      dplyr::select(-c(`row ID`:`row retention time`))
    
    variable_info <-
      variable_info %>%
      dplyr::rename(variable_id = "row ID",
                    mz = "row m/z",
                    rt = "row retention time") %>%
      dplyr::mutate(variable_id = as.character(variable_id))
    
    if (rt_unit == "minute") {
      variable_info$rt <-
        variable_info$rt * 60
    }
    
    sample_info <-
      data.frame(
        sample_id = colnames(expression_data),
        class = "no",
        check.names = FALSE
      )
    
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
    
    rownames(expression_data) <- variable_info$variable_id
    
    
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
      function_name = "convet_mzmine2mass_dataset()",
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

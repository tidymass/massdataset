#' @title extract_expression_data
#' @description Extract expression data.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object mass_dataset class object.
#' @return A data frame.
#' @export
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     variable_info = variable_info,
#'     sample_info = sample_info
#'   )
#'  expression_data2 =
#'  extract_expression_data(object = object)
#'  head(expression_data2)

extract_expression_data = function(object) {
  expression_data = object@expression_data %>%
    as.data.frame()
  expression_data
}


#' @title extract_sample_info
#' @description Extract sample information.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object mass_dataset class object.
#' @return A data frame.
#' @export
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'   )
#'  sample_info2 =
#'  extract_sample_info(object = object)
#'  head(sample_info2)

extract_sample_info = function(object) {
  sample_info = object@sample_info %>%
    as.data.frame()
  sample_info
}


#' @title extract_variable_info
#' @description Extract variable information.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object mass_dataset class object.
#' @return A data frame.
#' @export
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'   )
#'  variable_info2 =
#'  extract_variable_info(object = object)
#'  head(variable_info2)

extract_variable_info = function(object) {
  variable_info = object@variable_info %>%
    as.data.frame()
  
  if (nrow(object@annotation_table) != 0) {
    annotation_table =
      object@annotation_table %>%
      dplyr::group_by(variable_id) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup()
    
    variable_info =
      variable_info %>%
      dplyr::left_join(annotation_table %>%
                         dplyr::select(-c(ms2_files_id:ms2_spectrum_id)),
                       by = "variable_id")
  }
  variable_info
}

#' @title extract_variable_info_note
#' @description Extract variable information note.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object mass_dataset class object.
#' @return A data frame.
#' @export

extract_variable_info_note = function(object) {
  variable_info_note = object@variable_info_note %>%
    as.data.frame()
  variable_info_note
}


#' @title extract_sample_info_note
#' @description Extract sample information note.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object mass_dataset class object.
#' @return A data frame.
#' @export

extract_sample_info_note = function(object) {
  sample_info_note = object@sample_info_note %>%
    as.data.frame()
  sample_info_note
}

#' @title Extract process information
#' @description Extract process information
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @return A data.frame.
#' @export
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' library(massdataset)
#'
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'   )
#'
#' object
#' extract_process_info(object)

extract_process_info =
  function(object) {
    process_info = object@process_info
    return(process_info)
  }

#' @title Extract ms2 data
#' @description Extract ms2 data
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @return A ms2_data class object.
#' @export

extract_ms2_data =
  function(object) {
    ms2_data = object@ms2_data
    return(ms2_data)
  }


#####other functions

###sample_info
#' @rdname mass_dataset-class
#' @return sample_info

setMethod(
  f = "sample_info",
  signature = "mass_dataset",
  definition = function(object)
    object@sample_info
)

##expression_data
#' @rdname mass_dataset-class
#' @return expression_data

setMethod(
  f = "expression_data",
  signature = "mass_dataset",
  definition = function(object)
    object@expression_data
)



##variable_info
#' @rdname mass_dataset-class
#' @return variable_info
setMethod(
  f = "variable_info",
  signature = "mass_dataset",
  definition = function(object)
    object@variable_info
)


##process_info
#' @rdname mass_dataset-class
#' @return process_info
setMethod(
  f = "process_info",
  signature = "mass_dataset",
  definition = function(object)
    object@process_info
)


##ms2_data
#' @rdname mass_dataset-class
#' @return ms2_data
setMethod(
  f = "ms2_data",
  signature = "mass_dataset",
  definition = function(object)
    object@ms2_data
)

#' Extraction Functions for mass_dataset Objects
#'
#' This set of functions extracts various components from `mass_dataset` objects,
#' such as expression data, sample information, variable information, annotation table, etc.
#'
#' @name mass_dataset-extracting
NULL


#' Extract Components from mass_dataset Object
#'
#' @author Xiaotao Shen <shenxt1990@outlook.com>
#' @description This function extracts the components from an object 
#' of class "mass_dataset". It first checks if the object belongs to the 
#' class "mass_dataset" and then extracts the expression data.
#'
#' @param object An object of class "mass_dataset" from which the expression data will be extracted.
#'
#' @return A data frame containing the expression data or other componets in mass_dataset class.
#'
#' @export
#' @rdname mass_dataset-extracting
#' @examples
#' ###expression_data
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
  check_object_class(object = object, class = "mass_dataset")
  expression_data = object@expression_data %>%
    as.data.frame()
  expression_data
}


#' @author Xiaotao Shen <shenxt1990@outlook.com>
#' @param object An object of class "mass_dataset" from which the sample information will be extracted.
#'
#' @export
#' @rdname mass_dataset-extracting
#' @examples
#' ###sample_info
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
  check_object_class(object = object, class = "mass_dataset")
  sample_info = object@sample_info %>%
    as.data.frame()
  sample_info
}



#' @author Xiaotao Shen <shenxt1990@outlook.com>
#'
#' @param object An object of class "mass_dataset" from which the variable information will be extracted.
#' @param with_expression_data Logical, whether to include expression data in the output. Default is FALSE.
#'
#' @export
#' @rdname mass_dataset-extracting
#' @examples
#' ###variable_info
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

extract_variable_info = function(object,
                                 with_expression_data = FALSE) {
  check_object_class(object = object, class = "mass_dataset")
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
  
  if (with_expression_data) {
    expression_data <-
      slot(object, "expression_data") %>%
      tibble::rownames_to_column(var = "variable_id")
    variable_info <-
      variable_info %>%
      dplyr::left_join(expression_data,
                       by = "variable_id")
  }
  
  variable_info
}



#' @author Xiaotao Shen <shenxt1990@outlook.com>
#' @param object An object of class "mass_dataset" from which the annotation table will be extracted.
#' @param with_expression_data Logical, whether to include expression data in the output. Default is FALSE.
#' 
#' @export
#' @rdname mass_dataset-extracting
#' @examples
#' ###annotation_table
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'   )
#' annotation_table =
#'   extract_annotation_table(object = object)
#' head(annotation_table)

extract_annotation_table <-
  function(object,
           with_expression_data = FALSE) {
    check_object_class(object = object, class = "mass_dataset")
    annotation_table <- object@annotation_table %>%
      as.data.frame()
    if (nrow(annotation_table) > 0) {
      if (with_expression_data) {
        expression_data <-
          slot(object, "expression_data") %>%
          tibble::rownames_to_column(var = "variable_id")
        annotation_table <-
          annotation_table %>%
          dplyr::left_join(expression_data,
                           by = "variable_id")
      }
    }
    annotation_table
  }


#' @author Xiaotao Shen <shenxt1990@outlook.com>
#'
#' @param object An object of class "mass_dataset" from which the variable information notes will be extracted.
#'
#' @export
#' @rdname mass_dataset-extracting
#' @examples
#' ###variable_info_note
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'   )
#'  test =
#'  extract_variable_info_note(object = object)
#'  head(test)

extract_variable_info_note = function(object) {
  check_object_class(object = object, class = "mass_dataset")
  variable_info_note = object@variable_info_note %>%
    as.data.frame()
  variable_info_note
}



#' @author Xiaotao Shen <shenxt1990@outlook.com>
#' @param object An object of class "mass_dataset" from which the sample information notes will be extracted.
#'
#' @export
#' @rdname mass_dataset-extracting
#' @examples
#' ###sample_info_note
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'   )
#'  test =
#'  extract_sample_info_note(object = object)
#'  head(test)

extract_sample_info_note = function(object) {
  check_object_class(object = object, class = "mass_dataset")
  sample_info_note = object@sample_info_note %>%
    as.data.frame()
  sample_info_note
}

#'
#' @author Xiaotao Shen <shenxt1990@outlook.com>
#'
#' @param object An object of class "mass_dataset" from which the process information will be extracted.
#'
#' @export
#' @rdname mass_dataset-extracting
#' @examples
#' ###process_info
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
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
    check_object_class(object = object, class = "mass_dataset")
    process_info = object@process_info
    return(process_info)
  }


#' @author Xiaotao Shen <shenxt1990@outlook.com>
#' @param object An object of class "mass_dataset" from which the MS2 data will be extracted.
#'
#'
#' @export
#' @rdname mass_dataset-extracting
#' @examples
#' ###ms2_data
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'   )
#'  test =
#'  extract_ms2_data(object = object)

extract_ms2_data =
  function(object) {
    check_object_class(object = object, class = "mass_dataset")
    ms2_data = object@ms2_data
    return(ms2_data)
  }


####------------------------------------------------------------------------------
#####other functions
###sample_info
#' @method sample_info mass_dataset
#' @docType methods
#' @rdname mass_dataset-extracting
#' @param object (required) mass_dataset class object
#' @return A data.frame (sample_info)

setMethod(
  f = "sample_info",
  signature = "mass_dataset",
  definition = function(object)
    object@sample_info
)

##expression_data
#' @method expression_data mass_dataset
#' @docType methods
#' @rdname mass_dataset-extracting
#' @param object (required) mass_dataset class object
#' @return A data.frame (expression_data)

setMethod(
  f = "expression_data",
  signature = "mass_dataset",
  definition = function(object)
    object@expression_data
)

##variable_info
#' @method variable_info mass_dataset
#' @docType methods
#' @rdname mass_dataset-extracting
#' @param object (required) mass_dataset class object
#' @return A data.frame (variable_info)
setMethod(
  f = "variable_info",
  signature = "mass_dataset",
  definition = function(object)
    object@variable_info
)


##process_info
#' @method process_info mass_dataset
#' @docType methods
#' @rdname mass_dataset-extracting
#' @param object (required) mass_dataset class object
#' @return A list of tidymass_parameter class objects
setMethod(
  f = "process_info",
  signature = "mass_dataset",
  definition = function(object)
    object@process_info
)

##ms2_data
#' @method ms2_data mass_dataset
#' @docType methods
#' @rdname mass_dataset-extracting
#' @param object (required) mass_dataset class object
#' @return A ms2_data class object.
setMethod(
  f = "ms2_data",
  signature = "mass_dataset",
  definition = function(object)
    object@ms2_data
)

##sample_info_note
#' @method sample_info_note mass_dataset
#' @docType methods
#' @rdname mass_dataset-extracting
#' @param object (required) mass_dataset class object
#' @return A data.frame (sample_info_note)
setMethod(
  f = "sample_info_note",
  signature = "mass_dataset",
  definition = function(object)
    object@sample_info_note
)


##variable_info_note
#' @method variable_info_note mass_dataset
#' @docType methods
#' @rdname mass_dataset-extracting
#' @param object (required) mass_dataset class object
#' @return A data.frame (variable_info_note)
setMethod(
  f = "variable_info_note",
  signature = "mass_dataset",
  definition = function(object)
    object@variable_info_note
)


##annotation_table
#' @method annotation_table mass_dataset
#' @docType methods
#' @rdname mass_dataset-extracting
#' @param object (required) mass_dataset class object
#' @return A data.frame (annotation_table)
setMethod(
  f = "annotation_table",
  signature = "mass_dataset",
  definition = function(object)
    object@annotation_table
)


####replacement method for sample_info

"sample_info<-" <- function(object, value) {
  object
}

setReplaceMethod("sample_info", "mass_dataset", function(object, value) {
  object@sample_info <- value
  check_result <-
    check_mass_dataset(
      expression_data = object@expression_data,
      sample_info = object@sample_info,
      variable_info = object@variable_info,
      sample_info_note = object@sample_info_note,
      variable_info_note = object@variable_info_note
    )
  
  if (stringr::str_detect(check_result, "error")) {
    stop(check_result)
  }
  
  return(object)
  
})




####replacement method for variable_info

"variable_info<-" <- function(object, value) {
  object
}

setReplaceMethod("variable_info", "mass_dataset", function(object, value) {
  object@variable_info <- value
  check_result <-
    check_mass_dataset(
      expression_data = object@expression_data,
      sample_info = object@sample_info,
      variable_info = object@variable_info,
      sample_info_note = object@sample_info_note,
      variable_info_note = object@variable_info_note
    )
  
  if (stringr::str_detect(check_result, "error")) {
    stop(check_result)
  }
  
  return(object)
  
})







####replacement method for expression_data

"expression_data<-" <- function(object, value) {
  object
}

setReplaceMethod("expression_data", "mass_dataset", function(object, value) {
  object@expression_data <- value
  check_result <-
    check_mass_dataset(
      expression_data = object@expression_data,
      sample_info = object@sample_info,
      variable_info = object@variable_info,
      sample_info_note = object@sample_info_note,
      variable_info_note = object@variable_info_note
    )
  
  if (stringr::str_detect(check_result, "error")) {
    stop(check_result)
  }
  
  return(object)
  
})








####replacement method for sample_info_note
"sample_info_note<-" <- function(object, value) {
  object
}

setReplaceMethod("sample_info_note", "mass_dataset", function(object, value) {
  object@sample_info_note <- value
  check_result <-
    check_mass_dataset(
      expression_data = object@expression_data,
      sample_info = object@sample_info,
      variable_info = object@variable_info,
      sample_info_note = object@sample_info_note,
      variable_info_note = object@variable_info_note
    )
  
  if (stringr::str_detect(check_result, "error")) {
    stop(check_result)
  }
  
  return(object)
  
})


####replacement method for variable_info_note
"variable_info_note<-" <- function(object, value) {
  object
}

setReplaceMethod("variable_info_note", "mass_dataset", function(object, value) {
  object@variable_info_note <- value
  check_result <-
    check_mass_dataset(
      expression_data = object@expression_data,
      sample_info = object@sample_info,
      variable_info = object@variable_info,
      sample_info_note = object@sample_info_note,
      variable_info_note = object@variable_info_note
    )
  
  if (stringr::str_detect(check_result, "error")) {
    stop(check_result)
  }
  
  return(object)
  
})



####replacement method for ms2_data
"ms2_data<-" <- function(object, value) {
  object
}

setReplaceMethod(f = "ms2_data", "mass_dataset", function(object, value) {
  object@ms2_data <- value
  return(object)
})

####replacement method for process_info
"process_info<-" <- function(object, value) {
  object
}

setReplaceMethod("process_info", "mass_dataset", function(object, value) {
  object@process_info <- value
  return(object)
})






####replacement method for annotation_table
"annotation_table<-" <- function(object, value) {
  object
}

setReplaceMethod("annotation_table", "mass_dataset", function(object, value) {
  object@annotation_table <- value
  return(object)
})

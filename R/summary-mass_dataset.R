#' Get Some Information of a mass_dataset Object
#'
#' These functions returns the information of a mass_dataset object.
#' @rdname mass_dataset-summary
#' @param object A mass_dataset object.
#'
#'
#' @author Xiaotao Shen \email{shenxt1990@@outlook.com}
#'
#' @export
#' @examples
#' ###get_sample_number
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'   )
#'  get_sample_number(object = object)

get_sample_number = function(object) {
  check_object_class(object = object, class = "mass_dataset")
  object@expression_data %>%
    as.data.frame() %>%
    ncol()
}


#' Get the Number of Variables in a mass_dataset Object
#'
#' @rdname mass_dataset-summary
#' @param object A mass_dataset object.
#'
#'
#' @author Xiaotao Shen \email{shenxt1990@@outlook.com}
#'
#' @export
#' @examples
#' ###get_variable_number
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'   )
#'  get_variable_number(object = object)

get_variable_number = function(object) {
  check_object_class(object = object, class = "mass_dataset")
  object@expression_data %>%
    as.data.frame() %>%
    nrow()
}




#' Get the Sample IDs from a mass_dataset Object
#'
#' @param object A mass_dataset object.
#'
#'
#' @author Xiaotao Shen \email{shenxt1990@@outlook.com}
#'
#' @export
#' @rdname mass_dataset-summary
#' @examples
#' ###get_sample_id
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'   )
#'  get_sample_id(object = object)

get_sample_id = function(object) {
  check_object_class(object = object, class = "mass_dataset")
  object@expression_data %>%
    as.data.frame() %>%
    colnames()
}


#' Get the Variable IDs from a mass_dataset Object
#'
#' @rdname mass_dataset-summary
#' @param object A mass_dataset object.
#'
#'
#' @author Xiaotao Shen \email{shenxt1990@@outlook.com}
#'
#' @export
#' @examples
#' ###get_variable_id
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'   )
#'  head(get_variable_id(object = object))

get_variable_id = function(object) {
  check_object_class(object = object, class = "mass_dataset")
  object@expression_data %>%
    as.data.frame() %>%
    rownames()
}


#' Get the Number of Missing Values in a mass_dataset Object
#' @rdname mass_dataset-summary
#' @param object A mass_dataset object.
#' @param by A character string specifying how to count missing values. 
#'           Options are "total", "sample", or "variable". Default is "total".
#' @param show_by A character string specifying the format to show the result. 
#'                Options are "number" or "percentage". Default is "number".
#'
#' @author Xiaotao Shen \email{shenxt1990@@outlook.com}
#'
#' @export
#' @examples
#' ###get_mv_number
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'   )
#' object
#' head(get_variable_id(object = object))
#' get_mv_number(object)
#' get_mv_number(object, by = "sample")
#' head(get_mv_number(object, by = "variable", "percentage"))

get_mv_number =
  function(object,
           by = c("total", "sample", "variable"),
           show_by = c("number", "percentage")) {
    check_object_class(object = object, class = "mass_dataset")
    by = match.arg(by)
    show_by = match.arg(show_by)
    
    expression_data =
      object@expression_data %>%
      as.data.frame()
    
    total_mv_number =
      sum(is.na(expression_data))
    
    sample_mv_number =
      apply(expression_data, 2, function(x) {
        sum(is.na(x))
      })
    
    variable_mv_number =
      apply(expression_data, 1, function(x) {
        sum(is.na(x))
      })
    
    if (by == "total") {
      if (show_by == "number") {
        return(total_mv_number)
      } else{
        return(total_mv_number / (nrow(expression_data) * ncol(expression_data)))
      }
    }
    
    if (by == "sample") {
      if (show_by == "number") {
        return(sample_mv_number)
      } else{
        return(sample_mv_number / nrow(expression_data))
      }
    }
    
    if (by == "variable") {
      if (show_by == "number") {
        return(variable_mv_number)
      } else{
        return(variable_mv_number / ncol(expression_data))
      }
    }
  }


#' @title dim
#' @method dim mass_dataset
#' @param x A mass_dataset object.
#' @export
#' @return A named integer vector with the dimensions of the mass_dataset object. 
#' The names of the dimensions are "variables" and "samples".
dim.mass_dataset <- function(x) {
  # info <-
  #   paste0(
  #     paste(dim(x@expression_data)[2], "samples"),
  #     " x ",
  #     paste(dim(x@expression_data)[1], "variables")
  #   )
  # info
  info <-
    dim(extract_expression_data(x))
  names(info) <- c("variables", "samples")
  info
}

#' @title nrow
#' @method nrow mass_dataset
#' @param x A mass_dataset object.
#' @export
#' @return A named integer representing the number of variables in the mass_dataset object.
#' The name of the integer is "variables".
nrow.mass_dataset <- function(x) {
  info <-
    nrow(x@expression_data)
  names(info) <- "variables"
  info
}

#' @title ncol
#' @method ncol mass_dataset
#' @param x A mass_dataset object.
#' @export
#' @return A named integer representing the number of samples in the mass_dataset object.
#' The name of the integer is "samples".
ncol.mass_dataset <- function(x) {
  info <-
    ncol(x@expression_data)
  names(info) <- 
    "samples"
  info
}

#' @title colnames
#' @method colnames mass_dataset
#' @param x A mass_dataset object.
#' @export
#' @return A character vector containing the column names (sample IDs) of the 
#' expression_data slot in the mass_dataset object.

colnames.mass_dataset =
  function(x) {
    colnames(x@expression_data)
  }


#' @title rownames
#' @method rownames mass_dataset
#' @param x A mass_dataset object.
#' @export
#' @return A character vector containing the row names (variable IDs) of 
#' the expression_data slot in the mass_dataset object.

rownames.mass_dataset =
  function(x) {
    rownames(x@expression_data)
  }

###old version
# #' @title apply
# #' @method apply mass_dataset
# #' @param X X
# #' @param MARGIN MARGIN
# #' @param FUN FUN
# #' @param ... ...
# #' @param simplify simplify
# #' @export
# #' @rdname mass_dataset-summary
# #' @return result
#
# apply.mass_dataset =
#   function(X, MARGIN, FUN, ..., simplify = TRUE){
#     apply(as.matrix(X@expression_data), MARGIN, FUN, ..., simplify = simplify)
#   }

#' @title apply
#' @method apply mass_dataset
#' @param X A mass_dataset object.
#' @param MARGIN An integer vector indicating which margins should be "swept out". 
#' 1 indicates rows, 2 indicates columns.
#' @param FUN The function to be applied. This function should take a data vector 
#' argument and return a result of length one.
#' @param ... Additional arguments to FUN.
#' @param simplify Logical; should the result be simplified to 
#' a vector or matrix if possible?
#' @export
#' @return The result of applying FUN to the margins of X. If simplify is TRUE, 
#' then the result may be a vector or matrix.

setMethod(f = "apply",
          signature(X = "mass_dataset"),
          function (X, MARGIN, FUN, ..., simplify = TRUE) {
            apply(as.matrix(X@expression_data),
                  MARGIN, FUN, ..., simplify = simplify)
          })


# #' @title intersect
# #' @method intersect mass_dataset
# #' @param x x
# #' @param y y
# #' @export
# #' @return result
# 
# intersect.mass_dataset = function(x, y){
#   intersect(x@sample_info$sample_id,
#             y@sample_info$sample_id)
# }



#' @title intersect
#' @method intersect mass_dataset
#' @param x A mass_dataset object.
#' @param y Another mass_dataset object.
#' @export
#' @return A vector containing the common sample IDs between the 
#' two mass_dataset objects.

setMethod(f = "intersect",
          signature(x = "mass_dataset",
                    y = "mass_dataset"),
          function (x, y) {
            intersect(x@sample_info$sample_id,
                      y@sample_info$sample_id)
          })


#' @title summary
#' @method summary mass_dataset
#' @param object A mass_dataset object.
#' @param ... Additional arguments to be passed to the `summary` 
#' function for the expression data.
#' @export
summary.mass_dataset <- function(object, ...) {
  summary(object@expression_data, ...)
}

#' @title length
#' @method length mass_dataset
#' @param x A `mass_dataset` object whose `expression_data` slot's length 
#' you want to find.
#' @export
#' @return The length of the `expression_data` slot in the 
#' `mass_dataset` object.

length.mass_dataset = function(x) {
  length(x@expression_data)
}


#' @title names
#' @method names mass_dataset
#' @param x A `mass_dataset` object whose `expression_data` 
#' slot's names you want to retrieve.
#' @export
#' @return The names of the elements in the `expression_data`
#'  slot of the `mass_dataset` object.

names.mass_dataset = function(x) {
  names(x@expression_data)
}

#' @title dimnames
#' @method dimnames mass_dataset
#' @param x A `mass_dataset` object whose `expression_data` slot's dimension 
#' names you want to retrieve.
#' @export
#' @return The dimension names (row and column names) of 
#' the `expression_data` slot in the `mass_dataset` object.

dimnames.mass_dataset = function(x) {
  dimnames(x@expression_data)
}


#' @title is.na
#' @method is.na mass_dataset
#' @param x A `mass_dataset` object whose `expression_data` 
#' slot you want to check for missing values.
#' 
#' @export
#' @return A logical matrix of the same dimensions as `expression_data`, 
#' indicating the presence of missing values.
is.na.mass_dataset <- function(x) {
  is.na(x@expression_data)
}

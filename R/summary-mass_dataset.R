#' @title get_sample_number
#' @description Number of samples
#' @docType methods
#' @rdname summary-mass_dataset
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) A mass_dataset class object.
#' @return A numeric.
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
#'  get_sample_number(object = object)

get_sample_number = function(object) {
  check_object_class(object = object, class = "mass_dataset")
  object@expression_data %>%
    as.data.frame() %>%
    ncol()
}


#' @title get_variable_number
#' @description Number of variables
#' @docType methods
#' @rdname summary-mass_dataset
#' @param object (required) mass_dataset class object.
#' @return A numeric.
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
#'  get_variable_number(object = object)

get_variable_number = function(object) {
  check_object_class(object = object, class = "mass_dataset")
  object@expression_data %>%
    as.data.frame() %>%
    nrow()
}




#' @title get_sample_id
#' @description Sample IDs
#' @docType methods
#' @rdname summary-mass_dataset
#' @param object (required) mass_dataset class object.
#' @return A character vector.
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
#'  get_sample_id(object = object)

get_sample_id = function(object) {
  check_object_class(object = object, class = "mass_dataset")
  object@expression_data %>%
    as.data.frame() %>%
    colnames()
}


#' @title get_variable_id
#' @description Variable IDs
#' @docType methods
#' @rdname summary-mass_dataset
#' @param object (required) mass_dataset class object.
#' @return A character vector.
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
#'  head(get_variable_id(object = object))

get_variable_id = function(object) {
  check_object_class(object = object, class = "mass_dataset")
  object@expression_data %>%
    as.data.frame() %>%
    rownames()
}


#' @title get_mv_number
#' @description Get missing value number/percentage in expression data.
#' @docType methods
#' @rdname summary-mass_dataset
#' @param object (required) mass_dataset class object.
#' @param by total: Missing value number in total. sample: Missing value number in each sample.
#' variable: Missing value number in each variable.
#' @param show_by number: missing value number. percentage: missing value percentage.
#' @return A numeric (vector).
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
#' @param x x
#' @export
#' @rdname summary-mass_dataset
#' @return message
dim.mass_dataset <- function(x) {
  dim(x@expression_data)
}

#' @title nrow
#' @method nrow mass_dataset
#' @param x x
#' @export
#' @rdname summary-mass_dataset
#' @return message
nrow.mass_dataset <- function(x) {
  nrow(x@expression_data)
}

#' @title ncol
#' @method ncol mass_dataset
#' @param x x
#' @export
#' @rdname summary-mass_dataset
#' @return message
ncol.mass_dataset <- function(x) {
  ncol(x@expression_data)
}

#' @title colnames
#' @method colnames mass_dataset
#' @param x x
#' @export
#' @rdname summary-mass_dataset
#' @return message

colnames.mass_dataset =
  function(x){
    colnames(x@expression_data)
  }


#' @title rownames
#' @method rownames mass_dataset
#' @param x x
#' @export
#' @rdname summary-mass_dataset
#' @return message

rownames.mass_dataset =
  function(x) {
    rownames(x@expression_data)
  }

#' ###old version
#' #' @title apply
#' #' @method apply mass_dataset
#' #' @param X X
#' #' @param MARGIN MARGIN
#' #' @param FUN FUN
#' #' @param ... ...
#' #' @param simplify simplify
#' #' @export
#' #' @rdname summary-mass_dataset
#' #' @return result
#' 
#' apply.mass_dataset = 
#'   function(X, MARGIN, FUN, ..., simplify = TRUE){
#'     apply(as.matrix(X@expression_data), MARGIN, FUN, ..., simplify = simplify)
#'   }

#' @title apply
#' @method apply mass_dataset
#' @param X X
#' @param MARGIN MARGIN
#' @param FUN FUN
#' @param ... ...
#' @param simplify simplify
#' @export
#' @rdname summary-mass_dataset
#' @return result

setMethod(f = "apply",
          signature(X = "mass_dataset"),
          function (X, MARGIN, FUN, ..., simplify = TRUE) {
            apply(as.matrix(X@expression_data), 
                  MARGIN, FUN, ..., simplify = simplify)
          })


#' @title intersect
#' @method intersect mass_dataset
#' @param x x
#' @param y y
#' @export
#' @rdname summary-mass_dataset
#' @return mass_dataset object

intersect.mass_dataset = function(x, y){
  intersect(x@sample_info$sample_id,
            y@sample_info$sample_id)
}


#' @title summary
#' @method summary mass_dataset
#' @param object object
#' @param ... other parameters
#' @rdname summary-mass_dataset
#' @export
summary.mass_dataset <- function(object, ...) {
  summary(object@expression_data, ...)
}

#' @title length
#' @method length mass_dataset
#' @param x x
#' @export
#' @rdname summary-mass_dataset
#' @return vector object

length.mass_dataset = function(x){
  length(x@expression_data)
}


#' @title names
#' @method names mass_dataset
#' @param x x
#' @export
#' @rdname summary-mass_dataset
#' @return vector object

names.mass_dataset = function(x){
  names(x@expression_data)
}



#' @title dimnames
#' @method dimnames mass_dataset
#' @param x x
#' @export
#' @rdname summary-mass_dataset
#' @return vector object

dimnames.mass_dataset = function(x){
  dimnames(x@expression_data)
}





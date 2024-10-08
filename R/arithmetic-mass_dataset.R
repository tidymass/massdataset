#' Log-transform Expression Data in `mass_dataset` Object
#'
#' This function applies a log transformation to the expression data
#' contained within a `mass_dataset` object.
#'
#' @param x A `mass_dataset` object containing the expression data.
#' @param base The base of the logarithm. The default is `exp(1)`, 
#'   which corresponds to the natural logarithm.
#'
#' @details
#' The `log.mass_dataset` function takes a `mass_dataset` object as its input, 
#' and applies a log transformation to its expression data. This can be useful
#' for various downstream analyses that assume or benefit from log-transformed data.
#' The function also updates the `process_info` slot of the `mass_dataset` 
#' object to include information about the log transformation.
#'
#' @return
#' Returns a `mass_dataset` object with log-transformed expression data.
#'
#' @examples
#' # Assuming 'md' is a 'mass_dataset' object
#' # log_transformed_md <- log(md, base = 2)
#'
#' @author Xiaotao Shen \email{shenxt1990@@outlook.com}
#' @export
#' @rdname arithmetic-mass_dataset

log.mass_dataset = function(x, base = exp(1)) {
  expression_data = x@expression_data
  expression_data = log(expression_data, base = base)
  x@expression_data = expression_data
  process_info = x@process_info
  parameter <- new(
    Class = "tidymass_parameter",
    pacakge_name = "base",
    function_name = "log()",
    parameter = list("base" = base),
    time = Sys.time()
  )
  if (all(names(process_info) != "log")) {
    process_info$log = parameter
  } else{
    process_info$log = c(process_info$log, parameter)
  }
  x@process_info = process_info
  return(x)
}


#' Absolute Value Transformation of Expression Data in `mass_dataset` Object
#'
#' This function applies an absolute value transformation to the expression data
#' contained within a `mass_dataset` object.
#'
#' @param x A `mass_dataset` object containing the expression data to be transformed.
#'
#' @details
#' The `abs.mass_dataset` function takes a `mass_dataset` object as its input 
#' and applies an absolute value transformation to its expression data. This can 
#' be useful in scenarios where negative values in the dataset need to be transformed 
#' to their positive counterparts for subsequent analyses.
#' 
#' Additionally, the function updates the `process_info` slot of the `mass_dataset` 
#' object to capture details about the absolute value transformation process.
#'
#' @return
#' Returns a `mass_dataset` object with expression data transformed to absolute values.
#'
#' @examples
#' # Assuming 'md' is a 'mass_dataset' object
#' # abs_transformed_md <- abs.mass_dataset(md)
#'
#' @author Xiaotao Shen \email{shenxt1990@@outlook.com}
#' @export
#' @rdname arithmetic-mass_dataset

abs.mass_dataset = function(x) {
  expression_data = x@expression_data
  
  expression_data = abs(expression_data)
  
  x@expression_data = expression_data
  
  process_info = x@process_info
  
  parameter <- new(
    Class = "tidymass_parameter",
    pacakge_name = "base",
    function_name = "abs()",
    parameter = list(),
    time = Sys.time()
  )
  
  if (all(names(process_info) != "abs")) {
    process_info$abs = parameter
  } else{
    process_info$abs = c(process_info$abs, parameter)
  }
  
  x@process_info = process_info
  
  return(x)
}


#' Square Root Transformation for mass_dataset
#'
#' @author Xiaotao Shen <shenxt1990@outlook.com>
#' @description This function takes a `mass_dataset` object and applies the square root transformation
#' to the expression data stored in the object. It also updates the `process_info` attribute to log the transformation.
#' 
#' @param x A `mass_dataset` object containing the `expression_data` and `process_info` attributes.
#' 
#' @return A `mass_dataset` object with updated `expression_data` and `process_info`.
#' 
#' @examples
#' \dontrun{
#' # Assuming 'dataset' is a mass_dataset object
#' transformed_dataset <- sqrt.mass_dataset(dataset)
#' }
#' 
#' @details
#' 1. Extracts the `expression_data` from the `mass_dataset` object.
#' 2. Applies the square root transformation to the `expression_data`.
#' 3. Updates the `expression_data` in the `mass_dataset` object.
#' 4. Logs the transformation in the `process_info` attribute, including the package name, function name, parameters, and time.
#' @rdname arithmetic-mass_dataset


sqrt.mass_dataset = function(x) {
  expression_data = x@expression_data
  
  expression_data = sqrt(expression_data)
  
  x@expression_data = expression_data
  
  process_info = x@process_info
  
  parameter <- new(
    Class = "tidymass_parameter",
    pacakge_name = "base",
    function_name = "sqrt()",
    parameter = list(),
    time = Sys.time()
  )
  
  if (all(names(process_info) != "sqrt")) {
    process_info$sqrt = parameter
  } else{
    process_info$sqrt = c(process_info$sqrt, parameter)
  }
  
  x@process_info = process_info
  
  return(x)
}

#' @title scale
#' @method scale mass_dataset
#' @param x x
#' @param center center
#' @param scale scale
#' @export
#' @rdname arithmetic-mass_dataset
#' @return mass_dataset object

scale.mass_dataset =
  function(x, center = TRUE, scale = TRUE) {
    expression_data = x@expression_data
    
    expression_data = scale(t(expression_data), center = center,
                            scale = scale) %>%
      t() %>%
      as.data.frame()
    
    x@expression_data = expression_data
    
    process_info = x@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "base",
      function_name = "scale()",
      parameter = list("center" = center,
                       "scale" = scale),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "scale")) {
      process_info$scale = parameter
    } else{
      process_info$scale = c(process_info$scale, parameter)
    }
    
    x@process_info = process_info
    
    return(x)
  }


#' @title +
#' @method + mass_dataset
#' @param e1 a mass_dataset class object
#' @param e2 numeric
#' @export
#' @rdname arithmetic-mass_dataset
#' @return mass_dataset object

setMethod(f = "+",
          signature(e1 = "mass_dataset", e2 = "numeric"),
          function (e1, e2) {
            e1@expression_data = e1@expression_data + e2
            
            process_info = e1@process_info
            
            parameter <- new(
              Class = "tidymass_parameter",
              pacakge_name = "base",
              function_name = "+",
              parameter = list(),
              time = Sys.time()
            )
            
            if (all(names(process_info) != "plus")) {
              process_info$plus = parameter
            } else{
              process_info$plus = c(process_info$plus, parameter)
            }
            
            e1@process_info = process_info
            return(e1)
          })


#' @title -
#' @method - mass_dataset
#' @param e1 a mass_dataset class object
#' @param e2 numeric
#' @export
#' @rdname arithmetic-mass_dataset
#' @return mass_dataset object

setMethod(f = "-",
          signature(e1 = "mass_dataset", e2 = "numeric"), function (e1, e2) {
            e1@expression_data = e1@expression_data - e2
            
            process_info = e1@process_info
            
            parameter <- new(
              Class = "tidymass_parameter",
              pacakge_name = "base",
              function_name = "-",
              parameter = list(),
              time = Sys.time()
            )
            
            if (all(names(process_info) != "minus")) {
              process_info$minus = parameter
            } else{
              process_info$minus = c(process_info$minus, parameter)
            }
            e1@process_info = process_info
            return(e1)
          })


#' @title *
#' @method * mass_dataset
#' @param e1 a mass_dataset class object
#' @param e2 numeric
#' @export
#' @rdname arithmetic-mass_dataset
#' @return mass_dataset object

setMethod(f = "*",
          signature(e1 = "mass_dataset", e2 = "numeric"), function (e1, e2) {
            e1@expression_data = e1@expression_data * e2
            
            process_info = e1@process_info
            
            parameter <- new(
              Class = "tidymass_parameter",
              pacakge_name = "base",
              function_name = "*",
              parameter = list(),
              time = Sys.time()
            )
            
            if (all(names(process_info) != "times")) {
              process_info$times = parameter
            } else{
              process_info$times = c(process_info$times, parameter)
            }
            e1@process_info = process_info
            return(e1)
          })


#' @title /
#' @method / mass_dataset
#' @param e1 a mass_dataset class object
#' @param e2 numeric
#' @export
#' @rdname arithmetic-mass_dataset
#' @return mass_dataset object

setMethod(f = "/",
          signature(e1 = "mass_dataset", e2 = "numeric"), function (e1, e2) {
            e1@expression_data = e1@expression_data / e2
            
            process_info = e1@process_info
            
            parameter <- new(
              Class = "tidymass_parameter",
              pacakge_name = "base",
              function_name = "/",
              parameter = list(),
              time = Sys.time()
            )
            
            if (all(names(process_info) != "divide")) {
              process_info$divide = parameter
            } else{
              process_info$divide = c(process_info$divide, parameter)
            }
            e1@process_info = process_info
            return(e1)
          })

#' @title `>`
#' @method > mass_dataset
#' @param e1 a mass_dataset class object
#' @param e2 numeric
#' @export
#' @rdname arithmetic-mass_dataset
#' @return A logical data.frame

setMethod(f = ">",
          signature(e1 = "mass_dataset", e2 = "numeric"),
          function (e1, e2) {
            e1@expression_data > e2
          })

#' @title `>=`
#' @method >= mass_dataset
#' @param e1 a mass_dataset class object
#' @param e2 numeric
#' @export
#' @rdname arithmetic-mass_dataset
#' @return A logical data.frame

setMethod(f = ">=",
          signature(e1 = "mass_dataset", e2 = "numeric"),
          function (e1, e2) {
            e1@expression_data >= e2
          })

#' @title <
#' @method < mass_dataset
#' @param e1 a mass_dataset class object
#' @param e2 numeric
#' @export
#' @rdname arithmetic-mass_dataset
#' @return A logical data.frame

setMethod(f = "<",
          signature(e1 = "mass_dataset", e2 = "numeric"),
          function (e1, e2) {
            e1@expression_data < e2
          })

#' @title <=
#' @method <= mass_dataset
#' @param e1 a mass_dataset class object
#' @param e2 numeric
#' @export
#' @rdname arithmetic-mass_dataset
#' @return A logical data.frame

setMethod(f = "<=",
          signature(e1 = "mass_dataset", e2 = "numeric"),
          function (e1, e2) {
            e1@expression_data <= e2
          })

#' @title ==
#' @method == mass_dataset
#' @param e1 a mass_dataset class object
#' @param e2 numeric
#' @export
#' @rdname arithmetic-mass_dataset
#' @return A logical data.frame

setMethod(f = "==",
          signature(e1 = "mass_dataset", e2 = "numeric"),
          function (e1, e2) {
            e1@expression_data == e2
          })

# #' @title colSums
# #' @method colSums mass_dataset
# #' @param x x
# #' @param na.rm na.rm
# #' @param dims dims
# #' @export
# #' @rdname arithmetic-mass_dataset
# #' @return vector object
#
# colSums.mass_dataset = function(x, na.rm = FALSE, dims = 1){
#   colSums(x@expression_data, na.rm = na.rm, dims = dims)
# }



#' @title colSums
#' @method colSums mass_dataset
#' @param x x
#' @param na.rm na.rm
#' @param dims dims
#' @export
#' @rdname arithmetic-mass_dataset
#' @return result

setMethod(f = "colSums",
          signature(x = "mass_dataset"),
          function (x, na.rm = FALSE, dims = 1) {
            colSums(x@expression_data, na.rm = na.rm, dims = dims)
          })

# #' @title rowSums
# #' @method rowSums mass_dataset
# #' @param x x
# #' @param na.rm na.rm
# #' @param dims dims
# #' @export
# #' @rdname arithmetic-mass_dataset
# #' @return vector object
#
# rowSums.mass_dataset = function(x, na.rm = FALSE, dims = 1){
#   rowSums(x@expression_data, na.rm = na.rm, dims = dims)
# }



#' @title rowSums
#' @method rowSums mass_dataset
#' @param x x
#' @param na.rm na.rm
#' @param dims dims
#' @export
#' @rdname arithmetic-mass_dataset
#' @return vector object

setMethod(f = "rowSums",
          signature(x = "mass_dataset"),
          function (x, na.rm = FALSE, dims = 1) {
            rowSums(x@expression_data, na.rm = na.rm, dims = dims)
          })



# #' @title colMeans
# #' @method colMeans mass_dataset
# #' @param x x
# #' @param na.rm na.rm
# #' @param dims dims
# #' @export
# #' @rdname arithmetic-mass_dataset
# #' @return vector object
# 
# colMeans.mass_dataset = function(x, na.rm = FALSE, dims = 1){
#   colMeans(x@expression_data, na.rm = na.rm, dims = dims)
# }


#' @title colMeans
#' @method colMeans mass_dataset
#' @param x x
#' @param na.rm na.rm
#' @param dims dims
#' @export
#' @rdname arithmetic-mass_dataset
#' @return vector object

setMethod(f = "colMeans",
          signature(x = "mass_dataset"),
          function (x, na.rm = FALSE, dims = 1) {
            colMeans(x@expression_data, na.rm = na.rm, dims = dims)
          })



# #' @title rowMeans
# #' @method rowMeans mass_dataset
# #' @param x x
# #' @param na.rm na.rm
# #' @param dims dims
# #' @export
# #' @rdname arithmetic-mass_dataset
# #' @return vector object
#
# rowMeans.mass_dataset = function(x, na.rm = FALSE, dims = 1){
#   rowMeans(x@expression_data, na.rm = na.rm, dims = dims)
# }



#' @title rowMeans
#' @method rowMeans mass_dataset
#' @param x x
#' @param na.rm na.rm
#' @param dims dims
#' @export
#' @rdname arithmetic-mass_dataset
#' @return vector object

setMethod(f = "rowMeans",
          signature(x = "mass_dataset"),
          function (x, na.rm = FALSE, dims = 1) {
            rowMeans(x@expression_data, na.rm = na.rm, dims = dims)
          })

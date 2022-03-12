#' @title log
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @method log mass_dataset
#' @param x x
#' @param base base
#' @export
#' @rdname arithmetic-mass_dataset
#' @return mass_dataset object

log.mass_dataset = function(x, base = exp(1)){
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


#' @title abs
#' @method abs mass_dataset
#' @param x x
#' @export
#' @rdname arithmetic-mass_dataset
#' @return mass_dataset object

abs.mass_dataset = function(x){
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


#' @title sqrt
#' @method sqrt mass_dataset
#' @param x x
#' @export
#' @rdname arithmetic-mass_dataset
#' @return mass_dataset object

sqrt.mass_dataset = function(x){
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
  function(x, center = TRUE, scale = TRUE){
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





#' #' @title colSums
#' #' @method colSums mass_dataset
#' #' @param x x
#' #' @param na.rm na.rm
#' #' @param dims dims
#' #' @export
#' #' @rdname arithmetic-mass_dataset
#' #' @return vector object
#' 
#' colSums.mass_dataset = function(x, na.rm = FALSE, dims = 1){
#'   colSums(x@expression_data, na.rm = na.rm, dims = dims)
#' }



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

#' #' @title rowSums
#' #' @method rowSums mass_dataset
#' #' @param x x
#' #' @param na.rm na.rm
#' #' @param dims dims
#' #' @export
#' #' @rdname arithmetic-mass_dataset
#' #' @return vector object
#' 
#' rowSums.mass_dataset = function(x, na.rm = FALSE, dims = 1){
#'   rowSums(x@expression_data, na.rm = na.rm, dims = dims)
#' }



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



#' #' @title colMeans
#' #' @method colMeans mass_dataset
#' #' @param x x
#' #' @param na.rm na.rm
#' #' @param dims dims
#' #' @export
#' #' @rdname arithmetic-mass_dataset
#' #' @return vector object
#' 
#' colMeans.mass_dataset = function(x, na.rm = FALSE, dims = 1){
#'   colMeans(x@expression_data, na.rm = na.rm, dims = dims)
#' }


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



#' #' @title rowMeans
#' #' @method rowMeans mass_dataset
#' #' @param x x
#' #' @param na.rm na.rm
#' #' @param dims dims
#' #' @export
#' #' @rdname arithmetic-mass_dataset
#' #' @return vector object
#' 
#' rowMeans.mass_dataset = function(x, na.rm = FALSE, dims = 1){
#'   rowMeans(x@expression_data, na.rm = na.rm, dims = dims)
#' }



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

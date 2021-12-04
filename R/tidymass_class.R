#' @title create_tidymass_class
#' @description Create tidymass object.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param expression_data MS1 peak table name. Columns are samples and rows are variables.
#' @param sample_info Sample information name.
#' @param variable_info MS1 peak table name. Columns are samples and rows are variables.
#' @param sample_info_note Sample information name.
#' @param variable_info_note Sample information name.
#' @return A tidymass-class object.
#' @export
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("sample_info_note")
#' data("variable_info")
#' data("variable_info_note")
#' object =
#'   create_tidymass_class(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'     sample_info_note = sample_info_note,
#'     variable_info_note = variable_info_note
#'   )
#'   object

# library(tidyverse)
# sxtTools::setwd_project()
# load("demo_data/expression_data")
# load("demo_data/sample_info")
# load("demo_data/variable_info")
# load("demo_data/sample_info_note")
# load("demo_data/variable_info_note")
# 
# object =
#   create_tidymass_class(
#     expression_data = expression_data,
#     sample_info = sample_info,
#     variable_info = variable_info,
#     sample_info_note = sample_info_note,
#     variable_info_note = variable_info_note
#   )
# 
# object
# 
# 
# save(object, file = "demo_data/object")

create_tidymass_class =
  function(expression_data,
           sample_info,
           variable_info,
           sample_info_note,
           variable_info_note) {
    
    check_result <-
      check_tidymass_class_data(
        expression_data = expression_data,
        sample_info = sample_info,
        variable_info = variable_info,
        sample_info_note = sample_info_note,
        variable_info_note = variable_info_note
      )
    
    if (stringr::str_detect(check_result, "error")) {
      stop(check_result)
    }
    
    if (missing(sample_info_note)) {
      sample_info_note = data.frame()
    }
    
    if (missing(variable_info_note)) {
      variable_info_note = data.frame()
    }
    
    process_info = list()
   

    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "create_tidymass_class()",
      parameter = list("no" = "no"),
      time = Sys.time()
    )
   
    process_info$Creation = parameter
     
    object <- new(
      Class = "tidymass",
      expression_data = expression_data,
      ms2_data = data.frame(),
      sample_info = sample_info,
      variable_info = variable_info,
      sample_info_note = sample_info_note,
      variable_info_note = variable_info_note,
      process_info = process_info,
      version = "0.9.1"
    )
    invisible(object)
  }


##S4 class for function tidymass-class
#' An S4 class that stores the MS dataset
#' @export
setClass(
  Class = "tidymass",
  representation(
    expression_data = "data.frame",
    ms2_data = "data.frame",
    sample_info = "data.frame",
    variable_info = "data.frame",
    sample_info_note = "data.frame",
    variable_info_note = "data.frame",
    process_info = "list",
    version = "character"
  )
)

####show method for tidymass
setMethod(
  f = "show",
  signature = "tidymass",
  definition = function(object) {
    # requireNamespace("magrittr")
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::green("massdataset version:", object@version, "\n"))
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::green("1.expression_data (extract_expression_data()):\n"))
    cat("[",
      nrow(object@expression_data),
      "x",
      ncol(object@expression_data),
      " data.frame]\n"
    )
    cat(crayon::green("2.sample_info (extract_sample_info()):\n"))
    cat("[",
        nrow(object@sample_info),
        "x",
        ncol(object@sample_info),
        "data.frame]\n"
    )
    cat(crayon::green("3.variable_info (extract_variable_info()):\n"))
    cat("[",
        nrow(object@variable_info),
        "x",
        ncol(object@variable_info),
        "data.frame]\n"
    )
    cat(crayon::green("4.sample_info_note (extract_sample_info_note()):\n"))
    cat("[",
        nrow(object@sample_info_note),
        "x",
        ncol(object@sample_info_note),
        "data.frame]\n"
    )
    cat(crayon::green("5.variable_info_note (extract_variable_info_note()):\n"))
    cat("[",
        nrow(object@variable_info_note),
        "x",
        ncol(object@variable_info_note),
        "data.frame]\n"
    )
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::green("Processing information (extract_process_info())\n"))
    if (.hasSlot(object = object, name = "process_info") &
        length(object@process_info) != 0) {
      process_info <- object@process_info
      
      for(idx in 1:length(process_info)){
        cat(crayon::green(names(process_info)[idx], paste(rep("-", 10), collapse = ""), "\n"))
        if(length(process_info[[idx]]) == 1){
          data.frame(
            "Package" = process_info[[idx]]@pacakge_name,
            "Function used" = process_info[[idx]]@function_name,
            "Time" = process_info[[idx]]@time
          ) %>% 
            print()
        }else{
          data.frame(
            "Package" = process_info[[idx]] %>% lapply(function(x)
              x@pacakge_name) %>% unlist(),
            "Function used" = process_info[[idx]] %>% lapply(function(x)
              x@function_name) %>% unlist(),
            "Time" = process_info[[idx]] %>% lapply(function(x)
              as.character(x@time)) %>% unlist()
          ) %>%
            print()  
        }
      }
    } else{
      cat(crayon::red("There are no processing for your data.\n"))
    }
  }
)


## cannonical location for dim, dimnames
#' @title dim
#' @rdname dim
#' @aliases dim
#' @param x object
#' @docType methods
#' @export
setMethod("dim", "tidymass",
          function(x)
          {
            dim(x@expression_data)
          })

#' @title nrow
#' @rdname nrow
#' @aliases nrow
#' @docType methods
#' @param x object
#' @export
setMethod("nrow", "tidymass",
          function(x)
          {
            nrow(x@expression_data)
          })

#' @title ncol
#' @rdname ncol
#' @aliases ncol
#' @docType methods
#' @param x object
#' @export
setMethod("ncol", "tidymass",
          function(x)
          {
            ncol(x@expression_data)
          })

#' @title colnames
#' @rdname colnames
#' @aliases colnames
#' @docType methods
#' @param x object
#' @export
setMethod("colnames", "tidymass",
          function(x)
          {
            colnames(x@expression_data)
          })

#' @title rownames
#' @rdname rownames
#' @aliases rownames
#' @param x object
#' @docType methods
#' @export
setMethod("rownames", "tidymass",
          function(x)
          {
            rownames(x@expression_data)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###
#' Extract parts of tidymass
#' @name [
#' @aliases [,tidymass-method
#' @docType methods
#' @rdname extract-methods
#' @param x tidymass class object
#' @param i row index
#' @param j column index
#' @param ... other parameters
#' @param drop drop or not.
#' @usage x[i,j,drop = FALSE,...]
#' @export

setMethod(f = "[", signature = c(x = "tidymass", i = "ANY", j = "ANY"),
          function(x, i, j, ..., drop = FALSE){
            if (missing(i) & missing(j)){
              return(x)
            }
            
            if (!missing(i)) {
              if (is.character(i)) {
                i <- match(i, rownames(x@expression_data))
              }
            }else{
              i = 1:nrow(x@expression_data)
            }
            
            if (!missing(j)) {
              if (is.character(j)) {
                j <- match(j, colnames(x@expression_data))
              }
            }else{
              j = 1:ncol(x@expression_data)
            }
            
            if(sum(is.na(j)) > 0){
              j = j[!is.na(j)]
              if(length(j) == 0){
                j = 1:ncol(x)
                warning("All sample index (j) are not in the object. Please check.")
              }else{
                warning("Some sample index (j) are not in the object. Please check.")
              }
            }
            
            if(any(!j %in% 1:ncol(x))){
              warning("Some sample index (j) are not in the object. Please check.")
              j = j[j %in% 1:ncol(x)]
            }
            
            if(sum(is.na(i)) > 0){
              i = i[!is.na(i)]
              if(length(i) == 0){
                i = 1:nrow(x)
                warning("Some variable index (i) are not in the object. Please check.")
              }else{
                warning("Some variable index (i) are not in the object. Please check.")
              }
            
            }
            
            if(any(!i %in% 1:nrow(x))){
              warning("Some variable index (i) are not in the object. Please check.")
              i = i[i %in% 1:nrow(x)]
            }

            ###add paramters
            ####add parameters
            process_info = x@process_info
            
            parameter <- new(
              Class = "tidymass_parameter",
              pacakge_name = "massdataset",
              function_name = "[",
              parameter = list("i" = i,
                               "j" = j),
              time = Sys.time()
            )
            
            if (all(names(process_info) != "Subset")) {
              process_info$Subset = parameter
            }else{
              process_info$Subset = c(process_info$Subset, parameter)  
            }
            
            object@process_info = process_info
            
            x@expression_data = x@expression_data[i,j,drop = drop]
            x@sample_info = x@sample_info[j,,drop = FALSE]
            x@variable_info = x@variable_info[i,,drop = FALSE]
            return(x)
          })



##S4 class for parameter
#' An S4 class that stores the parameters
#' @export
setClass(
  Class = "tidymass_parameter",
  representation(
    pacakge_name = "character",
    function_name = "character",
    parameter = "list",
    time = "POSIXct"
  )
)

####show method for tidymass_parameter
setMethod(
  f = "show",
  signature = "tidymass_parameter",
  definition = function(object) {
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::green("pacakge_name:", object@pacakge_name), "\n")
    cat(crayon::green("function_name:", object@function_name), "\n")
    cat(crayon::green("time:", object@time), "\n")
    cat(crayon::green("parameters:\n"))
    for(idx in 1:length(object@parameter)){
      cat(crayon::green(names(object@parameter)[idx], ":" ,object@parameter[idx]), "\n")
    }
  }
)


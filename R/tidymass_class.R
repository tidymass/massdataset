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
    process_info$Creation = list()
    process_info$Creation$function_used = "create_tidymass_class"
    process_info$Creation$parameters = "no"
    process_info$Creation$time = Sys.time()
    
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
    cat(crayon::green("Expression data\n"))
    cat(
      ncol(object@expression_data),
      "samples and",
      nrow(object@expression_data),
      "variables\n"
    )
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::green("Processing\n"))
    if (.hasSlot(object = object, name = "process_info") &
        length(object@process_info) != 0) {
      process_info <- object@process_info
      mapply(function(x, y) {
        cat(crayon::green(x, paste(rep("-", 10), collapse = ""), "\n"))
        y$time = as.character(y$time)
        y = as.data.frame(t(data.frame(unlist(y))))
        colnames(y) = c("Function used", "Parameter", "Time")
        rownames(y) <- NULL
        print(y)
      },
      x = names(process_info),
      y = process_info)
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

            
            x@expression_data = x@expression_data[i,j,drop = drop]
            x@sample_info = x@sample_info[j,,drop = FALSE]
            x@variable_info = x@variable_info[i,,drop = FALSE]
            return(x)
          })
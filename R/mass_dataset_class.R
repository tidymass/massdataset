#' @title create_mass_dataset
#' @description Create mass_dataset object.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param expression_data MS1 peak table name. Columns are samples and rows are variables.
#' @param sample_info Sample information name.
#' @param variable_info MS1 peak table name. Columns are samples and rows are variables.
#' @param sample_info_note Sample information name.
#' @param variable_info_note Sample information name.
#' @return A mass_dataset-class object.
#' @export
#' @examples
# data("expression_data")
# data("sample_info")
# data("sample_info_note")
# data("variable_info")
# data("variable_info_note")
# object =
#   create_mass_dataset(
#     expression_data = expression_data,
#     sample_info = sample_info,
#     variable_info = variable_info,
#     sample_info_note = sample_info_note,
#     variable_info_note = variable_info_note
#   )
#   object

create_mass_dataset =
  function(expression_data,
           sample_info,
           variable_info,
           sample_info_note,
           variable_info_note) {
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
      function_name = "create_mass_dataset()",
      parameter = list("no" = "no"),
      time = Sys.time()
    )
    
    process_info$Creation = parameter
    
    object <- new(
      Class = "mass_dataset",
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


##S4 class for function mass_dataset-class
#' An S4 class that stores the MS dataset
#' @name mass_dataset
#' @docType class
#' @slot expression_data expression_data
#' @slot ms2_data ms2_data
#' @slot sample_info sample_info
#' @slot variable_info variable_info
#' @slot sample_info_note sample_info_note
#' @slot variable_info_note variable_info_note
#' @slot process_info process_info
#' @slot version version
#' @slot activated activated
#' @export
setClass(
  Class = "mass_dataset",
  representation(
    expression_data = "data.frame",
    ms2_data = "data.frame",
    sample_info = "data.frame",
    variable_info = "data.frame",
    sample_info_note = "data.frame",
    variable_info_note = "data.frame",
    process_info = "list",
    version = "character",
    activated = "character"
  )
)

#' show method for mass_dataset class
#' @name show
#' @docType methods
#' @rdname show-methods
#' @title show method
#' @param object A \code{mass_dataset} instance.
#' @return message
#' @importFrom methods show
#' @author Xiaotao Shen
#' @export
setMethod(
  f = "show",
  signature = "mass_dataset",
  definition = function(object) {
    ###check again
    check_result = check_mass_dataset(
      expression_data = object@expression_data,
      sample_info = object@sample_info,
      variable_info = object@variable_info
    )
    if(check_result != "all good."){
      cat(crayon::red(check_result, "\n"))
      cat(crayon::red("You may changed the slots, try to use update_mass_dataset().\n"))
    }
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::green("massdataset version:", object@version, "\n"))
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::green("1.expression_data (extract_expression_data()):\n"))
    cat(
      "[",
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
        "data.frame]\n")
    cat(crayon::green("3.variable_info (extract_variable_info()):\n"))
    cat(
      "[",
      nrow(object@variable_info),
      "x",
      ncol(object@variable_info),
      "data.frame]\n"
    )
    cat(crayon::green("4.sample_info_note (extract_sample_info_note()):\n"))
    cat(
      "[",
      nrow(object@sample_info_note),
      "x",
      ncol(object@sample_info_note),
      "data.frame]\n"
    )
    cat(crayon::green("5.variable_info_note (extract_variable_info_note()):\n"))
    cat(
      "[",
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
      
      for (idx in 1:length(process_info)) {
        cat(crayon::green(names(process_info)[idx], paste(rep("-", 10), collapse = ""), "\n"))
        if (length(process_info[[idx]]) == 1) {
          data.frame(
            "Package" = process_info[[idx]]@pacakge_name,
            "Function used" = process_info[[idx]]@function_name,
            "Time" = process_info[[idx]]@time
          ) %>%
            print()
        } else{
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


#' @title dim
#' @rdname dim
#' @aliases dim
#' @param x object
#' @docType methods
#' @export
setMethod("dim", "mass_dataset",
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
setMethod("nrow", "mass_dataset",
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
setMethod("ncol", "mass_dataset",
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
setMethod("colnames", "mass_dataset",
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
setMethod("rownames", "mass_dataset",
          function(x)
          {
            rownames(x@expression_data)
          })
#'
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###
#' Extract parts of mass_dataset
#' @name [
#' @aliases [,mass_dataset-method
#' @docType methods
#' @rdname extract-methods
#' @param x mass_dataset class object
#' @param i row index
#' @param j column index
#' @param ... other parameters
#' @param drop drop or not.
#' @usage x[i,j,drop = FALSE,...]
#' @export

setMethod(f = "[",
          signature = c(x = "mass_dataset", i = "ANY", j = "ANY"),
          definition =
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

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

# library(tidyverse)
# sxtTools::setwd_project()
# rm(list = ls())
# expression_data = readr::read_csv("demo_data/Peak_table_for_cleaning.csv")
# sample_info = readr::read_csv("demo_data/sample_info.csv")
# 
# variable_info =
#   expression_data %>%
#   dplyr::select(name:rt) %>%
#   dplyr::rename(variable_id = name)
# 
# sample_info =
#   sample_info %>%
#   dplyr::rename(sample_id = sample.name)
# 
# rownames(expression_data) = expression_data$name
# 
# expression_data =
#   expression_data %>%
#   dplyr::select(-c(name:rt))
# 
# sample_info_note =
#   data.frame(name = colnames(sample_info),
#              meaning = colnames(sample_info))
# 
# variable_info_note =
#   data.frame(name = colnames(variable_info),
#              meaning = colnames(variable_info))
# 
# 
# rownames(expression_data) = variable_info$variable_id
# 
# object =
#   create_tidymass_class(expression_data = expression_data,
#                         sample_info = sample_info,
#                         variable_info = variable_info,
#                         sample_info_note = sample_info_note
#                         # variable_info_note = variable_info_note
#                         )
# 
# object

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
    
    if(missing(sample_info_note)){
      sample_info_note = data.frame()
    }
    
    if(missing(variable_info_note)){
      variable_info_note = data.frame()
    }
    
    process_info = list()
    process_info$Creation = list()
    process_info$Creation$function_used = "create_tidymass_class"
    process_info$Creation$parameters = "no"
    process_info$Creation$time = Sys.time()
    
    object <- new(Class = "tidymass",
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
setClass(
  Class = "tidymass",
  representation(
    expression_data = "data.frame",
    ms2_data = "data.frame",
    sample_info = "data.frame",
    variable_info = "list",
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
    cat(crayon::green("tidyTools version:", object@version, "\n"))
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::green("Expression data\n"))
    cat(ncol(object@expression_data), "samples and", nrow(object@expression_data), "variables\n")
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


#' #' @title Get data from tidymass object.
#' #' @description Get data from tidymass object.
#' #' @author Xiaotao Shen
#' #' \email{shenxt@@sioc.ac.cn}
#' #' @param object A tidymass object.
#' #' @param slot Class of data.
#' #' @return A data frame.
#' #' @export
#' 
#' get_data = function(object,
#'                     slot = c("Subject",
#'                              "QC",
#'                              "QC.DL",
#'                              "Blank",
#'                              "Tags",
#'                              "peak.table",
#'                              "sample.info")) {
#'   if (class(object) != "tidymass") {
#'     stop("Only the tidymass is supported!\n")
#'   }
#'   
#'   if (length(object@expression_data) > 1) {
#'     stop("Plase align batch first.\n")
#'   }
#'   
#'   # slot <- stringr::str_to_title(slot)
#'   slot <- match.arg(slot)
#'   
#'   if (slot == "Tags") {
#'     result <- object@expression_data[[1]] %>%
#'       dplyr::select(., -one_of(object@sample.info$sample.name))
#'     return(result)
#'   }
#'   
#'   if (slot == "peak.table") {
#'     result <- object@expression_data[[1]]
#'     return(result)
#'   }
#'   
#'   if (slot == "sample.info") {
#'     result <- object@sample.info
#'     return(result)
#'   }
#'   
#'   result <-
#'     try(dplyr::filter(.data = object@sample.info, class == slot)$sample.name %>%
#'           dplyr::select(.data = object@expression_data[[1]], .))
#'   
#'   if (ncol(result) == 0) {
#'     return(NULL)
#'   }
#'   return(result)
#' }
#' 
#' 
#' 
#' 
#' #' @title get_mv_plot_samples
#' #' @description get MV plot of subject samples.
#' #' @author Xiaotao Shen
#' #' \email{shenxt@@sioc.ac.cn}
#' #' @param object A tidymass object.
#' #' @param interactive interactive or not.
#' #' @return A ggplot2 object.
#' #' @export
#' 
#' get_mv_plot_samples = function(object,
#'                                interactive = FALSE) {
#'   if (class(object) != "tidymass") {
#'     stop("Only the tidymass is supported!\n")
#'   }
#'   plot <- try(object@process_info$filterSample$plot)
#'   if (class(plot)[1] == "try-error") {
#'     return(NULL)
#'   } else{
#'     if (interactive) {
#'       plotly::ggplotly(plot)
#'     } else{
#'       plot
#'     }
#'   }
#' }
#' 
#' #' @title calculate_rsd
#' #' @description Calculate RSD of peaks.
#' #' @author Xiaotao Shen
#' #' \email{shenxt@@sioc.ac.cn}
#' #' @param object A tidymass object.
#' #' @param slot Class of data.
#' #' @return A data frame with RSD.
#' #' @export
#' 
#' calculate_rsd = function(object,
#'                          slot = c("Subject",
#'                                   "QC",
#'                                   "QC.DL",
#'                                   "Blank",
#'                                   "Tags",
#'                                   "peak.table",
#'                                   "sample.info")) {
#'   slot <- match.arg(slot)
#'   if (class(object) != "tidymass") {
#'     stop("Only the tidymass is supported!\n")
#'   }
#'   
#'   data <- get_data(object = object, slot = slot)
#'   
#'   if (is.null(data)) {
#'     stop("No ", slot, " in your data.\n")
#'   }
#'   
#'   if (sum(is.na(data)) != 0) {
#'     stop("Please impute MV first!\n")
#'   }
#'   
#'   rsd <- apply(data, 1, function(x) {
#'     x <- as.numeric(x)
#'     sd(x) * 100 / mean(x)
#'   })
#'   
#'   rsd <- data.frame(
#'     index = 1:length(rsd),
#'     name = object@expression_data[[1]]$name,
#'     rsd,
#'     stringsAsFactors = FALSE
#'   )
#'   invisible(rsd)
#' }
#' 
#' 
#' 
#' #' @title get_parameters
#' #' @description Get parameters from a tidymass object.
#' #' @author Xiaotao Shen
#' #' \email{shenxt@@sioc.ac.cn}
#' #' @param object A tidymass object.
#' #' @return A data frame of parameters.
#' #' @export
#' 
#' get_parameters = function(object) {
#'   if (class(object) != "tidymass") {
#'     stop("Only the tidymass is supported!\n")
#'   }
#'   process_info <- object@process_info
#'   if (length(process_info) == 0) {
#'     cat(crayon::red("No process for this dataset.\n"))
#'     return(NULL)
#'   }
#'   
#'   process_info <-
#'     lapply(process_info, function(x) {
#'       x <- x[which(names(x) != "plot")]
#'       x <-
#'         x %>%
#'         unlist(.) %>%
#'         data.frame(., stringsAsFactors = FALSE) %>%
#'         data.frame(rownames(.), ., stringsAsFactors = FALSE)
#'       
#'       rownames(x) <- NULL
#'       colnames(x) <- c("Parameter", "Value")
#'       x
#'     })
#'   process_info
#' }

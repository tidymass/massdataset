
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
    if (check_result != "all good.") {
      cat(crayon::red(check_result, "\n"))
      cat(crayon::red(
        "You may changed the slots, try to use update_mass_dataset().\n"
      ))
    }
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::green("massdataset version:", object@version, "\n"))
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::green("1.expression_data:"))
    cat(
      "[",
      nrow(object@expression_data),
      "x",
      ncol(object@expression_data),
      "data.frame]\n"
    )
    cat(crayon::green("2.sample_info:"))
    cat("[",
        nrow(object@sample_info),
        "x",
        ncol(object@sample_info),
        "data.frame]\n")
    cat(crayon::green("3.variable_info:"))
    cat(
      "[",
      nrow(object@variable_info),
      "x",
      ncol(object@variable_info),
      "data.frame]\n"
    )
    cat(crayon::green("4.sample_info_note:"))
    cat(
      "[",
      nrow(object@sample_info_note),
      "x",
      ncol(object@sample_info_note),
      "data.frame]\n"
    )
    cat(crayon::green("5.variable_info_note:"))
    cat(
      "[",
      nrow(object@variable_info_note),
      "x",
      ncol(object@variable_info_note),
      "data.frame]\n"
    )
    cat(crayon::green("6.ms2_data:"))
    cat("[",
        sum(unlist(lapply(object@ms2_data, function(x) {
          length(unique(x@variable_id))
        }))),
        "variables x",
        sum(unlist(lapply(object@ms2_data, function(x) {
          length(unique(x@ms2_spectra))
        }))),
        "MS2 spectra]\n")
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::green("Processing information (extract_process_info())\n"))
    if (.hasSlot(object = object, name = "process_info") &
        length(object@process_info) != 0) {
      process_info <- object@process_info
      cat(crayon::green(length(process_info), "processings in total\n"))
      if(length(process_info) > 5){
        cat(crayon::green("Latest 3 processings show\n"))
        process_info <- tail(process_info, 3)
      }
      for (idx in seq_along(process_info)) {
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



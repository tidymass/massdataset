#' @title write_ms2_data mass_dataset
#' @description write_ms2_data mass_dataset
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @param file_type mgf or msp
#' @param file_name output file name
#' @param path path
#' @rdname mass_dataset-class
#' @return msp or mgf
#' @export

write_ms2_data =
  function(object,
           file_type = c("mgf", "msp"),
           file_name = "ms2_data",
           path = ".") {
    file_type = match.arg(file_type)
    
    file_name =
      paste(file_name,
            ".", file_type,
            sep = "")
    unlink(x = file.path(path, file_name),
           recursive = TRUE,
           force = TRUE)
    
    sink(file = file.path(path, file_name),
         append = TRUE)
    
    purrr::walk(
      .x = seq_along(object@variable_id),
      .f = function(temp_idx) {
        result =
          c(
            paste("TITLE=", object@variable_id[temp_idx], 
                  sep = ""),
            paste("MS2SPECTRUMID=", object@ms2_spectrum_id[temp_idx], 
                  sep = ""),
            paste("PEPMASS=", object@ms2_mz[temp_idx], 
                  sep = ""),
            paste("RTINSECONDS=", object@ms2_rt[temp_idx], 
                  sep = ""),
            paste("IONMODE=", object@polarity, 
                  sep = ""),
            paste("FILE=", object@ms2_file[temp_idx], 
                  sep = ""),
            paste("Num Peaks=", nrow(object@ms2_spectra[temp_idx][[1]]), 
                  sep = "")
          )
        
        single_spectra =
          object@ms2_spectra[temp_idx][[1]] %>%
          apply(1, function(x) {
            paste(x, collapse = " ")
          })
        
        result =
          c("BEGIN IONS", result, single_spectra, "END IONS", "", "")
        cat(
          result,
          file = file.path(path, file_name),
          append = TRUE,
          sep = "\n"
        )
      }
    )
    sink()
  }

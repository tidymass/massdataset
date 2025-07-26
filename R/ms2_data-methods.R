#' @title write_ms2_data
#' @description Write ms2_data class object to msp or mgf
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @param file_type (required) mgf or msp
#' @param file_name (required) output file name.
#' @param path (required) working directory.
#' @rdname ms2_data-class
#' @return msp or mgf
#' @export

write_ms2_data <-
  function(object,
           file_type = c("mgf", "msp"),
           file_name = "ms2_data",
           path = ".") {
    check_object_class(object = object, class = "ms2_data")
    file_type = match.arg(file_type)
    
    file_name =
      paste(file_name,
            ".", file_type,
            sep = "")
    unlink(x = file.path(path, file_name),
           recursive = TRUE,
           force = TRUE)
    
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
        writeLines(
          text = result,
          con = file.path(path, file_name),
          sep = "\n",
          useBytes = TRUE
        )
      }
    )
  }



#' @method filter ms2_data
#' @docType methods
#' @importFrom rlang quos !!!
#' @importFrom dplyr filter
#' @export
filter.ms2_data <- function(.data, ..., .preserve = FALSE) {
  dots <- rlang::quos(...)
  
  temp_data <-
    data.frame(
      variable_id = .data@variable_id,
      ms2_spectrum_id = .data@ms2_spectrum_id,
      ms2_mz = .data@ms2_mz,
      ms2_rt = .data@ms2_rt,
      ms2_file = .data@ms2_file
    )
  
  temp_data <-
    filter(temp_data, !!!dots, .preserve = .preserve)
  
  .data@variable_id <- temp_data$variable_id
  .data@ms2_spectrum_id <- temp_data$ms2_spectrum_id
  .data@ms2_mz <- temp_data$ms2_mz
  .data@ms2_rt <- temp_data$ms2_rt
  .data@ms2_file <- temp_data$ms2_file
  
  remain_idx <-
    which(names(.data@ms2_spectra) %in% temp_data$ms2_spectrum_id)
  
  .data@ms2_spectra <-
    .data@ms2_spectra[remain_idx]
  
  return(.data)
}

#' @importFrom dplyr filter
#' @export
dplyr::filter

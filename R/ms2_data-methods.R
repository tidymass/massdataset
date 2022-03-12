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

write_ms2_data =
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



#' setGeneric(
#'   name = "ms2_plot",
#'   def = function(spectrum1,
#'                  spectrum2,
#'                  spectrum1_name = "spectrum1",
#'                  spectrum2_name = "spectrum2",
#'                  range.mz,
#'                  ppm.tol = 30,
#'                  mz.ppm.thr = 400,
#'                  xlab = "Mass to charge ratio (m/z)",
#'                  ylab = "Relative intensity",
#'                  col1 = "red",
#'                  col2 = "black",
#'                  title.size = 15,
#'                  lab.size = 15,
#'                  axis.text.size = 15,
#'                  legend.title.size = 15,
#'                  legend.text.size = 15,
#'                  interactive_plot = FALSE) {
#'     standardGeneric("ms2_plot")
#'   }
#' )
#' 
#' #' @title ms2_plot
#' #' @method ms2_plot ms2_data
#' #' @param spectrum1 ms2_data
#' #' @param spectrum2 Spectrum 2.
#' #' @param spectrum1_name Name of spectrum1
#' #' @param spectrum2_name Name of spectrum2
#' #' @param range.mz range.mz
#' #' @param ppm.tol ppm.tol
#' #' @param mz.ppm.thr mz.ppm.thr
#' #' @param xlab xlab.
#' #' @param ylab ylab.
#' #' @param col1 Color 1.
#' #' @param col2 Color 2.
#' #' @param title.size title.size
#' #' @param lab.size lab.size
#' #' @param axis.text.size axis.text.size.
#' #' @param legend.title.size legend.title.size
#' #' @param legend.text.size legend.text.size
#' #' @param interactive_plot Interactive plot or not.
#' #' @export
#' #' @rdname methods-ms2_data
#' #' @return ms2 matching plot
#' 
#' setMethod(f = "ms2_plot",
#'           signature(spectrum1 = "mass_dataset"),
#'           function(spectrum1,
#'                    spectrum2,
#'                    spectrum1_name = "spectrum1",
#'                    spectrum2_name = "spectrum2",
#'                    range.mz,
#'                    ppm.tol = 30,
#'                    mz.ppm.thr = 400,
#'                    xlab = "Mass to charge ratio (m/z)",
#'                    ylab = "Relative intensity",
#'                    col1 = "red",
#'                    col2 = "black",
#'                    title.size = 15,
#'                    lab.size = 15,
#'                    axis.text.size = 15,
#'                    legend.title.size = 15,
#'                    legend.text.size = 15,
#'                    interactive_plot = FALSE) {
#'             masstools::ms2_plot(spectrum1 = spectrum1@ms2_spectra[[1]])
#'           })

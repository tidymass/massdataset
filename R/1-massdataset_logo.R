#' Display the MassDataset Logo
#'
#' This function displays the logo and version information for the massdataset package.
#'
#' @return None. This function is called for its side effect of printing the logo and messages.
#' @author Xiaotao Shen \email{shenxt1990@@outlook.com}
#' @importFrom stringr str_replace str_split str_replace_all str_trim
#' @importFrom ComplexHeatmap Heatmap columnAnnotation anno_barplot
#' @importFrom grid gpar
#' @importFrom dplyr filter mutate select everything left_join syms rename rename_with
#' @importFrom rstudioapi isAvailable hasFun getThemeInfo
#' @importFrom utils packageDescription write.csv
#' @importFrom cli rule symbol cli_abort
#' @importFrom crayon green blue col_align red black white style make_style num_colors
#' @importFrom openxlsx write.xlsx
#' @importFrom purrr map map2
#' @importFrom readr write_csv read_csv
#' @importFrom methods slot slot<-
#' @import ggplot2
#' @importFrom methods .hasSlot new is
#' @importFrom stats p.adjust rgamma sd median time as.formula lm residuals
#' @importFrom utils data str head tail packageVersion write.table read.delim
#' @importFrom utils read.table
#' @importFrom masstools read_mgf read_mzxml ms2_plot
#' @importFrom rlang warn quo_is_null abort seq2 caller_env check_dots_empty
#' @importFrom rlang as_quosure caller_arg current_env check_dots_empty0
#' @importFrom tibble add_column
#' @importClassesFrom S4Vectors DFrame
#' @importClassesFrom SummarizedExperiment SummarizedExperiment SimpleAssays
#' @export
#' @examples
#' massdataset_logo()

massdataset_logo <- function() {
  message("Thank you for using massdataset!")
  message("Version ",
          massdataset_version,
          " (",
          update_date,
          ')')
  message("More information: massdataset.tidymass.org")
  cat(
    c(
      "                          _____        _                 _   ",
      "                         |  __ \\      | |               | |  ",
      "  _ __ ___   __ _ ___ ___| |  | | __ _| |_ __ _ ___  ___| |_ ",
      " | '_ ` _ \\ / _` / __/ __| |  | |/ _` | __/ _` / __|/ _ \\ __|",
      " | | | | | | (_| \\__ \\__ \\ |__| | (_| | || (_| \\__ \\  __/ |_ ",
      " |_| |_| |_|\\__,_|___/___/_____/ \\__,_|\\__\\__,_|___/\\___|\\__|",
      "                                                             ",
      "                                                             "
    ),
    sep = "\n"
  )
}

massdataset_version <-
  as.character(utils::packageVersion(pkg = "massdataset"))

update_date <- as.character(Sys.time())

#' Retrieve the Version of MassDataset Package
#'
#' This function retrieves the current version of the massdataset package installed in the R environment.
#'
#' @return A character string indicating the version of the massdataset package.
#' @author Xiaotao Shen \email{shenxt1990@@outlook.com}
#' @examples
#' get_massdataset_version()
#' @export
get_massdataset_version = function() {
  return(as.character(utils::packageVersion(pkg = "massdataset")))
}

# library(cowsay)
# # https://onlineasciitools.com/convert-text-to-ascii-art
# # writeLines(capture.output(say("Hello"), type = "message"), con = "ascii_art.txt")
# art <- readLines("logo.txt")
# dput(art)
# massdataset_logo <-
#   c("                          _____        _                 _   ",
#     "                         |  __ \\      | |               | |  ",
#     "  _ __ ___   __ _ ___ ___| |  | | __ _| |_ __ _ ___  ___| |_ ",
#     " | '_ ` _ \\ / _` / __/ __| |  | |/ _` | __/ _` / __|/ _ \\ __|",
#     " | | | | | | (_| \\__ \\__ \\ |__| | (_| | || (_| \\__ \\  __/ |_ ",
#     " |_| |_| |_|\\__,_|___/___/_____/ \\__,_|\\__\\__,_|___/\\___|\\__|",
#     "                                                             ",
#     "                                                             "
#   )
# cat(massdataset_logo, sep = "\n")

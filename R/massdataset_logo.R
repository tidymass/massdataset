#' @title massdataset_logo
#' @description Get the detailed information of massdataset package.
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @importFrom stringr str_replace str_split str_replace_all str_trim
#' @importFrom ComplexHeatmap Heatmap columnAnnotation anno_barplot
#' @importFrom grid gpar
#' @importFrom ggplotify as.ggplot
#' @importFrom dplyr filter mutate select everything left_join
#' @importFrom plyr dlply .
#' @importFrom rstudioapi isAvailable hasFun getThemeInfo
#' @importFrom utils packageDescription write.csv
#' @importFrom cli rule symbol
#' @import leaflet
#' @importFrom crayon green blue col_align red black white style make_style num_colors
#' @importFrom plotly ggplotly
#' @importFrom pbapply pblapply
#' @importFrom openxlsx write.xlsx
#' @importFrom purrr map map2
#' @importFrom readr write_csv read_csv
#' @importFrom methods slot slot<-
#' @import ggplot2
#' @importFrom methods .hasSlot new is
#' @importFrom stats p.adjust rgamma sd median time
#' @importFrom utils data str head tail
#' @importFrom magrittr %>%
#' @importFrom ggsci pal_lancet
#' @importFrom masstools read_mgf read_mzxml
#' @importFrom rlang warn quo_is_null abort seq2
#' @export
#' @return logo
#' @examples
#' massdataset_logo()

massdataset_logo <- function() {
  message(crayon::green("Thank you for using massdataset!\n"))
  message(crayon::green("Version", massdataset_version, "(", update_date, ')\n'))
  message(crayon::green("More information: search 'tidymass massdataset'.\n"))
  cat(crayon::green(
    c(
      "                          _____        _                 _   ",
      "                         |  __ \\      | |               | |  ",
      "  _ __ ___   __ _ ___ ___| |  | | __ _| |_ __ _ ___  ___| |_ ",
      " | '_ ` _ \\ / _` / __/ __| |  | |/ _` | __/ _` / __|/ _ \\ __|",
      " | | | | | | (_| \\__ \\__ \\ |__| | (_| | || (_| \\__ \\  __/ |_ ",
      " |_| |_| |_|\\__,_|___/___/_____/ \\__,_|\\__\\__,_|___/\\___|\\__|",
      "                                                             ",
      "                                                             "
    )
    
  ), sep = "\n")
}

massdataset_version = "0.99.20"
update_date = as.character(Sys.time())

#' @title get_massdataset_version
#' @description Get massdataset package version
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @export
#' @return version
#' @examples
#' get_massdataset_version()
get_massdataset_version = function() {
  return(massdataset_version)
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

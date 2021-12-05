#' @title massdataset_logo
#' @description Get the detailed of massdataset package.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @importFrom stringr str_replace str_split str_replace_all str_trim
#' @importFrom dplyr filter mutate select everything
#' @importFrom rstudioapi isAvailable hasFun getThemeInfo
#' @importFrom utils packageDescription write.csv
#' @importFrom cli rule symbol
#' @importFrom crayon green blue col_align red black white style make_style num_colors
#' @importFrom plotly ggplotly
#' @importFrom pbapply pblapply
#' @importFrom openxlsx write.xlsx
#' @importFrom purrr map map2
#' @importFrom readr write_csv read_csv
#' @importFrom methods slot slot<-
#' @import ggplot2
#' @importFrom methods .hasSlot new
#' @importFrom stats p.adjust rgamma sd
#' @importFrom utils data str
#' @importFrom magrittr %>%
#' @export
#' @examples
#' massdataset_logo()

massdataset_logo <- function(){
  cat(crayon::green("Thank you for using massdataset!\n"))
  cat(crayon::green("Version 0.0.1 (2021-11-24)\n"))
  cat(crayon::green("More information can be found at https://tidymass.github.io/massdataset/\n"))
  cat(crayon::green(
    c("                          _____        _                 _   ", 
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



# library(cowsay)
# # https://onlineasciitools.com/convert-text-to-ascii-art
# # writeLines(capture.output(say("Hello"), type = "message"), con = "ascii_art.txt")
# art <- readLines("logo.txt")
# dput(art)
# metid_logo <-
#   c("                          _____        _                 _   ", 
#     "                         |  __ \\      | |               | |  ", 
#     "  _ __ ___   __ _ ___ ___| |  | | __ _| |_ __ _ ___  ___| |_ ", 
#     " | '_ ` _ \\ / _` / __/ __| |  | |/ _` | __/ _` / __|/ _ \\ __|", 
#     " | | | | | | (_| \\__ \\__ \\ |__| | (_| | || (_| \\__ \\  __/ |_ ", 
#     " |_| |_| |_|\\__,_|___/___/_____/ \\__,_|\\__\\__,_|___/\\___|\\__|", 
#     "                                                             ", 
#     "                                                             "
#   )
# cat(metid_logo, sep = "\n")

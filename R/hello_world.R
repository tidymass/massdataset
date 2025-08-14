#' Print “hello world”
#'
#' A minimal demo function that prints “hello world” to the console.
#'
#' @return Invisibly returns the string "hello world".
#' @examples
#' hello_world()
#' @export

hello_world <- function() {
  msg <- "hello world"
  print(msg)
  invisible(msg)
}
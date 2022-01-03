#' @method count mass_dataset
#' @docType methods
#' @importFrom rlang quos !!! !! enquo
#' @importFrom dplyr count
#' @export
count.mass_dataset <-
  function(x,
           ...,
           wt = NULL,
           sort = FALSE,
           name = NULL) {
    dots <- rlang::quos(...)
    
    if (length(x@activated) == 0) {
      stop("activate you object using activate_mass_dataset first.\n")
    }
    
    x =
      slot(object = x, name = x@activated)
    
    count(x,
          !!!dots,
          wt = !!enquo(wt),
          sort = sort,
          name = name)
  }



#' @method tally mass_dataset
#' @docType methods
#' @importFrom rlang quos !!! !! enquo
#' @importFrom dplyr tally
#' @export
tally.mass_dataset <-
  function(x,
           wt = NULL,
           sort = FALSE,
           name = NULL) {

    if (length(x@activated) == 0) {
      stop("activate you object using activate_mass_dataset first.\n")
    }
    
    x =
      slot(object = x, name = x@activated)
    
    tally(x,
          wt = !!enquo(wt),
          sort = sort,
          name = name)
  }

#' @importFrom dplyr count
#' @export
dplyr::count



#' @importFrom dplyr count
#' @export
dplyr::count

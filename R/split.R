#' @method split mass_dataset
#' @docType methods
#' @export
split.mass_dataset <- function(x, f, drop = FALSE, ...) {
  
  activated <-
    slot(object = .data,
         name = "activated")
  
  if (length(activated) == 0) {
    stop("activate you object using activate_mass_dataset() first.\n")
  }
  
  temp_slot <-
    slot(object = .data,
         name = activated)
  if(length(f) == 1){
    if(f %in% colnames(temp_slot))
      split(temp_slot, f = temp_slot[,f, drop = TRUE], drop = drop, ...)
  }else{
    split(temp_slot, f = f, drop = drop, ...)
  }
  
}


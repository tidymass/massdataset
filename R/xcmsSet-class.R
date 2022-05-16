############################################################
## xcmsSet
##
##S4 class for function xcmsSet-class
#' An S4 class that stores the MS dataset from xcms (Prof. Steffen Neumann)
#' @docType class
#' @slot peaks peaks
#' @slot groups groups
#' @slot groupidx groupidx
#' @slot filled filled
#' @slot phenoData phenoData
#' @slot rt rt
#' @slot filepaths filepaths
#' @slot profinfo profinfo
#' @slot dataCorrection dataCorrection
#' @slot polarity polarity
#' @slot progressInfo progressInfo
#' @slot progressCallback progressCallback
#' @slot mslevel mslevel
#' @slot scanrange scanrange
#' @slot .processHistory .processHistory
#' @exportClass xcmsSet
setClass(
  "xcmsSet",
  representation = representation(
    peaks = "matrix",
    groups = "matrix",
    groupidx = "list",
    filled = "numeric",
    phenoData = "data.frame",
    rt = "list",
    filepaths = "character",
    profinfo = "list",
    dataCorrection = "numeric",
    polarity = "character",
    progressInfo = "list",
    progressCallback = "function",
    mslevel = "numeric",
    scanrange = "numeric",
    .processHistory = "list"
  ),
  prototype = prototype(
    peaks = matrix(nrow = 0, ncol = 0),
    groups = matrix(nrow = 0, ncol = 0),
    groupidx = list(),
    filled = integer(0),
    phenoData = data.frame(),
    rt = list(),
    filepaths = character(0),
    profinfo = vector("list"),
    dataCorrection = integer(0),
    polarity = character(0),
    progressInfo = list(),
    mslevel = numeric(0),
    scanrange = numeric(0),
    progressCallback = function(progress)
      NULL,
    .processHistory = list()
  ),
  validity = function(object) {
    msg <- character()
    ## Check if all slots are present.
    slNames <- slotNames(object)
    missingSlots <- character()
    for (i in 1:length(slNames)) {
      if (!.hasSlot(object, slNames[i]))
        missingSlots <- c(missingSlots, slNames[i])
    }
    if (length(missingSlots) > 0)
      msg <- c(
        msg,
        paste0(
          "This xcmsSet lacks slot(s): ",
          paste(missingSlots, collapse = ","),
          ". Please update the object using",
          " the 'updateObject' method."
        )
      )
    ## Check the .processHistory slot.
    if (!any(missingSlots == ".processHistory")) {
      inh <- unlist(lapply(
        object@.processHistory,
        FUN = function(z) {
          return(inherits(z, "ProcessHistory"))
        }
      ))
      if (!all(inh))
        msg <- c(
          msg,
          paste0(
            "Slot '.processHistory' should",
            " only contain 'ProcessHistory'",
            " objects!"
          )
        )
    }
    if (length(msg))
      return(msg)
    return(TRUE)
  }
)
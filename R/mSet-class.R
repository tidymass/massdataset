## S4 class for function mSet-class
# This class is kept for backward compatibility, but massdataset does not
# define or import the external MSnbase classes used in old versions.
# Use generic slot types here so package loading/documentation does not fail.
#' An S4 class that stores the MS dataset from MetaboAnalystR (Prof. Jeff Xia)
#' @docType class
#' @slot rawfiles character
#' @slot rawOnDisk Raw MS data stored on disk.
#' @slot rawInMemory Raw MS data stored in memory.
#' @slot params params
#' @slot peakpicking peakpicking
#' @slot peakgrouping peakgrouping
#' @slot peakRTcorrection peakRTcorrection
#' @slot peakfilling peakfilling
#' @slot peakAnnotation peakAnnotation
#' @slot dataSet dataSet
#' @slot runningplan runningplan
#' @slot msgSet msgSet
#' @slot userpath userpath
#' @slot WorkingDir WorkingDir
#' @exportClass mSet

setClass(
  "mSet",
  representation = representation(
    rawfiles = "character",
    rawOnDisk = "ANY",
    rawInMemory = "ANY",
    params = "list",
    peakpicking = "list",
    peakgrouping = "list",
    peakRTcorrection = "list",
    peakfilling = "list",
    peakAnnotation = "list",
    dataSet = "data.frame",
    runningplan = "list",
    msgSet = "list",
    userpath = "character",
    WorkingDir = "character"
  ),
  prototype = prototype(
    rawfiles = vector("character"),
    rawOnDisk = NULL,
    rawInMemory = NULL,
    params = vector("list"),
    peakpicking = vector("list"),
    peakgrouping = vector("list"),
    peakRTcorrection = vector("list"),
    peakfilling = vector("list"),
    peakAnnotation = vector("list"),
    dataSet = data.frame(),
    runningplan = vector("list"),
    msgSet = vector("list"),
    userpath = character(0),
    WorkingDir = vector("character")
  ),
  validity = function(object) {
    return(TRUE)
  }
)

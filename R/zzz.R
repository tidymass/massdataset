.onAttach <- function(libname, pkgname) {
  needed <- core[!is_attached(core)]
  if (length(needed) == 0)
    return()
  
  crayon::num_colors(TRUE)
  massdataset_attach()
  
  
  packageStartupMessage(
    crayon::green(
      "massdataset,
More information can be found at https://tidymass.github.io/massdataset/
Authors: Xiaotao Shen (shenxt@stanford.edu)
Maintainer: Xiaotao Shen"
    )
  )
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}




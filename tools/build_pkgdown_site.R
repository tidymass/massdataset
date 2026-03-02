patch_pandoc_no_highlight <- function() {
  ns <- asNamespace("rmarkdown")
  name <- "pandoc_highlight_args"
  original <- get(name, envir = ns)

  unlockBinding(name, ns)
  assign(
    name,
    function(highlight, default = "tango") {
      if (is.null(highlight)) {
        return("--syntax-highlighting=none")
      }
      original(highlight, default = default)
    },
    envir = ns
  )
  lockBinding(name, ns)

  function() {
    unlockBinding(name, ns)
    assign(name, original, envir = ns)
    lockBinding(name, ns)
  }
}

suppressPackageStartupMessages({
  library(pkgdown)
  library(rmarkdown)
})

restore_patch <- patch_pandoc_no_highlight()
on.exit(restore_patch(), add = TRUE)

pkgdown::build_site(new_process = FALSE, install = FALSE)

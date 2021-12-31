#' @method relocate mass_dataset
#' @importFrom rlang quos !!! !! enquo
#' @importFrom dplyr relocate any_of
#' @importFrom tidyselect eval_select
#' @importFrom vctrs vec_unique
#' @export
relocate.mass_dataset <-
  function(.data,
           ...,
           .before = NULL,
           .after = NULL) {
    
    if (length(.data@activated) == 0) {
      stop("activate you object using activate_mass_dataset first.\n")
    }
    
    x =
      slot(object = .data, name = .data@activated)
    
    to_move <- tidyselect::eval_select(expr(c(...)), x)
    
    .before <- enquo(.before)
    .after <- enquo(.after)
    has_before <- !quo_is_null(.before)
    has_after <- !quo_is_null(.after)
    
    if (has_before && has_after) {
      abort("Must supply only one of `.before` and `.after`.")
    } else if (has_before) {
      where <- min(unname(tidyselect::eval_select(.before, x)))
      if (!where %in% to_move) {
        to_move <- c(to_move, where)
      }
    } else if (has_after) {
      where <- max(unname(tidyselect::eval_select(.after, x)))
      if (!where %in% to_move) {
        to_move <- c(where, to_move)
      }
    } else {
      where <- 1L
      if (!where %in% to_move) {
        to_move <- c(to_move, where)
      }
    }
    
    lhs <- setdiff(seq2(1, where - 1), to_move)
    rhs <- setdiff(seq2(where + 1, ncol(x)), to_move)
    
    pos <- vctrs::vec_unique(c(lhs, to_move, rhs))
    out <- x[pos]
    new_names <- names(pos)
    
    if (!is.null(new_names)) {
      names(out)[new_names != ""] <- new_names[new_names != ""]
    }
    
    slot(object = .data, name = .data@activated) = out
    
    if (.data@activated == "expression_data") {
      .data@sample_info = .data@sample_info[match(colnames(out), .data@sample_info$sample_id), ]
    }
    
    return(.data)
  }


#' @importFrom dplyr relocate
#' @export
dplyr::relocate

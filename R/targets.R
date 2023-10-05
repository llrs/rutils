#' Functions helping with targets
#'
#' Prepare a target and serve it to the user
#' @param name Name of the targets to prepare and load or read.
#' @param ... Other arguments passed around
#'
#' @return Depending of what [tar_load()] and [tar_read()] returns.
#' @export
#' @rdname llrs_tar
#' @name targets
llrs_tar_load <- function(name, ...){
  check_targets()
  targets::tar_make(names = name, ...)
  targets::tar_load(names = name, ...)
}

#' @rdname llrs_tar
#' @export
llrs_tar_read <- function(name, ...){
  check_targets()
  targets::tar_make(names = name, ...)
  targets::tar_read(name = name, ...)
}


check_targets <- function() {
  if (isFALSE(requireNamespace("targets", quietly = TRUE))) {
    stop("Please install targets to use this function.")
  }
}

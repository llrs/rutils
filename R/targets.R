#' Functions helping with targets
#'
#' Prepare a target and serve it to the user
#' @param name Name of the targets to prepare and load or read.
#' @param ... Other arguments passed around
#'
#' @return Depending of what [targets::tar_load()] and [targets::tar_read()] returns.
#' @export
#' @rdname llrs_tar
#' @name targets
llrs_tar_load <- function(name, ...){
  check_installed("targets")
  targets::tar_make(names = name, ...)
  targets::tar_load(names = name, ...)
}

#' @rdname llrs_tar
#' @export
llrs_tar_read <- function(name, ...){
  check_installed("targets")
  targets::tar_make(names = name, ...)
  targets::tar_read(name = name, ...)
}

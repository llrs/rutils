
#' @name Bioinformatics
#' @title Bioinformatics
#' @description
#' Some scripts are present at [llrs/aligning](https://github.com/llrs/aligning/).
#'
#' See the functions from `llrs_cnag` among others.
NULL


#' Create positions of a box
#'
#' Create a data.frame with the positions of the sample in a square box
#' @param size A numeric value with the number of rows or columns.
#'
#' @return A data.frame with the rows and columns.
#' @export
#' @examples
#' box10 <- llrs_box(10)
#' box9 <- llrs_box(9)
llrs_box <- function(size){
  stopifnot(is.numeric(size) && size %% 1 == 0L)
  sl <- seq_len(size)
  data.frame(fila =  rep(LETTERS[sl], each = size), columna = rep(sl))
}

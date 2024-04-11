obligatory_columns <- function(x, columns, obligatory = 1) {
  stopifnot(is.numeric(obligatory))
  coln <- colnames(x)
  all(coln %in% columns) && all(columns[obligatory] %in% coln)
}


is_dir <- function(path) {
  file.info(path)$isdir
}

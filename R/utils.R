obligatory_columns <- function(x, columns, obligatory = 1) {
  stopifnot(is.numeric(obligatory))
  coln <- colnames(x)
  all(coln %in% columns) && all(columns[obligatory] %in% coln)
}


is_dir <- function(path) {
  file.info(path)$isdir
}

check_installed <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    stop("Please install the ", paste(sQuote(x), collapse = " or "),
         " package.", call. = FALSE)
  }
  TRUE
}

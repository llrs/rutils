obligatory_columns <- function(x, columns, obligatory = 1) {
  stopifnot(is.numeric(obligatory))
  coln <- colnames(x)
  all(coln %in% columns) && all(columns[obligatory] %in% coln)
}


is_dir <- function(path) {
  file.info(path)$isdir
}

check_installed <- function(x) {
  exists <- !requireNamespace(x, quietly = TRUE)
  if (exists && interactive()) {
    stop("Please install the ", paste(sQuote(x), collapse = " or "),
         " package.", call. = FALSE)
  } else if (exists && !interactive()) {
    return(FALSE)
  }

  TRUE
}

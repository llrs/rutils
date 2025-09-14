# Append to .Rbuildignore
add2buildignore <- function(text, path = ".") {
  file_name <- ".Rbuildignore"
  path_file <- file.path(path, file_name)
  if (!file.exists(path_file) && file.path(path, "DESCRIPTION")) {
    file.create(path_file)
  }
  if (!endsWith(text, "\n")) {
    text <- paste0(text, "\n")
  }
  # Only add the new ignore if it is not present
  lines <- readLines(path_file)
  if (any(text %in% lines)) {
    return(NULL)
  }
  cat(text, file = path_file, append = TRUE)
}

# Append to .Rbuildignore
add2buildignore <- function(text, path = ".") {
  file_name <- ".Rbuildignore"
  path_file <- file.path(path, file_name)
  if (!file.exists(path_file) && file.path(path, "DESCRIPTION")) {
    file.create(path_file)
  }
  cat(text, file = path_file, append = TRUE)
}

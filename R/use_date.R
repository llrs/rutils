#' Use date
#'
#' Change the date of the version of the yaml.
#' Meant to work with CRAN TaskViews.
#'
#' @param file Path to the file to replace the date.
#' @param date The date to add date.
#'
#' @returns NULL
#' @export
use_date <- function(file, date = Sys.Date()) {
  if (!file.exists(file)) {
    stop("File doesn't exists")
  }
  if (!check_installed("rmarkdown")) {
    md <- rmarkdown::yaml_front_matter(file)
    if (date < as.Date(md$version)) {
      warning("New date is before the current date")
    }
  }
  if (!check_installed("xfun")) {
    xfun::gsub_file(file,
                    pattern = "version: .*",
                    replacement = sprintf("version : %s", date))
  }
  invisible(file)
}

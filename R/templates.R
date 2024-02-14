# See this: https://rstudio.github.io/rstudio-extensions/rstudio_project_templates.html
# In case there is need to create templates for documents/reports...


file_name <- function(project, ...) {
  date <- format(Sys.Date(), "%Y%m%d")
  out <- paste(date, project, ..., sep = "_")
  gsub("\\s+", "-", out)
}

#' Creates path for a figure
#'
#' creates a standardized name for figures and checks if the standardized
#' folders exist.
#' @param project Name of the project to be used in the title.
#' @param ... Other names/variables. The extensions of the file should be
#' included on the last element.
#'
#' @return A path to a file in a folder
#' @export
#' @examples
#' # llrs_figures("rutils", "pca", "sample1.png")
#' # llrs_figures("rutils", "pca", "sample whatever.png")
llrs_figures <- function(project, ...) {
  fn <- file_name(project, ...)
  fp <- file.path("out", "figures")
  out <- normalizePath(fp, mustWork = TRUE)
  file.path(out, fn)
}

#' Creates path for results
#'
#' It creates a standardized name for results and checks that the standardized
#' folders exist.
#' @param project Name of the project to be used in the title
#' @param ... Other names/variables. The extensions of the file should be
#' included on the last element.
#'
#' @return A path to a file.
#' @export
#' @examples
#' # llrs_results("rutils", "DEG", "limma", "a vs b.xlsx")
#' # llrs_results("rutils", "DEG", "pathways.xlsx")
llrs_results <- function(project, ...) {
  fn <- file_name(project, ...)
  fp <- file.path("out")
  out <- normalizePath(fp, mustWork = TRUE)
  file.path(out, fn)
}

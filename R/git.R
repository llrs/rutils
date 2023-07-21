#' Create shiny app
#'
#' Create a shiny app in the right place as well as set the local git repository
#' to synchronize.
#' @param project Name of the project. It can be also multiple folders.
#' @param path Path to the folder where it should be.
#' @param dest Path to the shiny folder used to serve the website.
#'
#' @return Called by its side effects. It will open a new RStudio project.
#' @export
llrs_shiny_create <- function(project, path = "~/ShinyApps/", dest = "/srv/shiny-server/") {
  norm_path <- normalizePath(dest, project)
  basename(norm_path)
  # Permission to edit the user and the group and others can read and execute
  if (dir.exists(norm_path)) {
    stop("Destination path already exists.")
  }
  dc <- dir.create(norm_path, recursive = TRUE, mode = "755")
  if (!dc) {
    stop("Problems creating the directory of the project at ", norm_path)
  }

  setwd(norm_path)
  system2("git", "init --shared=true .")
  system2("git", "config --local receive.denyCurrentBranch updateInstead")
  if (dir.exists(path)) {
    stop("Path already exists")
  }
  dc2 <- dir.create(path, recursive = TRUE, mode = "755")
  if (!dc2) {
    stop("Problems creating the directory of the project at ", path)
  }

  setwd(path)

  system2("git", paste("clone", norm_path))
  if (!check_rstudio()) {
    return(TRUE)
    stop("Missing rstudio.")
  }
  info <- rstudioapi::versionInfo()
  if (info$version < "1.1.287") {
    stop("Not working. Please update Rstudio.")
  }
  rstudioapi::initializeProject(norm_path)
  rstudioapi::openProject(norm_path, FALSE)
}

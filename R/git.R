#' Create shiny app
#'
#' Create a shiny app in the right place as well as set the local git repository
#' to synchronize.
#' @param project Name of the project. It can be also multiple folders.
#' @param path Path to the folder where it should be.
#' @param dest Path to the shiny folder used to serve the website.
#' @seealso [llrs_hook()]
#' @return Called by its side effects. It will open a new RStudio project.
#' @export
llrs_shiny_create <- function(project, path = "~/ShinyApps/", dest = "/srv/shiny-server/") {
  norm_path <- normalizePath(file.path(dest, project), mustWork = FALSE)
  # Permission to edit the user and the group and others can read and execute

  stopifnot("Use only one project" = length(path) == 1 &&
              length(dest) == 1 &&
              length(project) == 1)
  if (dir.exists(norm_path)) {
    stop("Destination path already exists.")
  }
  dc <- dir.create(norm_path, recursive = TRUE, mode = "755")
  if (!dc) {
    stop("Problems creating the directory of the project at ", norm_path)
  }

  old <- setwd(norm_path)
  on.exit(setwd(old), add = TRUE)
  system2("git", "init --shared=true .", stdout = FALSE, stderr = FALSE)
  system2("git", "config --local receive.denyCurrentBranch updateInstead",
          stdout = FALSE, stderr = FALSE)

  setwd(path)
  new_proj <- normalizePath(file.path(path, project), mustWork = FALSE)
  if (!dir.exists(new_proj)) {
    system2("git", paste("clone", norm_path), stderr = FALSE, stdout = FALSE)
  } else {
    setwd(new_proj)
    system2("git", paste("remote add master", norm_path), stderr = FALSE,
            stdout = FALSE)
  }

  if (!check_rstudio()) {
    warning("Missing rstudio.")
    return(TRUE)
  }
  info <- rstudioapi::versionInfo()
  if (info$version < "1.1.287") {
    stop("Not working. Please update Rstudio.")
  }
  rstudioapi::initializeProject(new_proj)
  rstudioapi::openProject(new_proj, TRUE)
}

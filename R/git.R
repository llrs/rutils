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
    system2("git", paste("remote add server", norm_path), stderr = FALSE,
            stdout = FALSE)
    system2("git", "push -u server master")
    message("Update the server")
    system2("git", "push")
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



#' Check package version
#'
#' @param repo Github repository.
#' @param path Local path to the repository.
#'
#' @returns TRUE if successfull
#' @export
#'
#' @examples
#' suppressWarnings(llrs_check_pkg_version("llrs/rutils"))
llrs_check_pkg_version <- function(repo, path = ".") {
  # Local repo
  stopifnot("Just one path" = length(path) == 1L)
  desc_file <- file.path(path, "DESCRIPTION")
  if (file.exists(desc_file)) {
    local_desc <- read.dcf(desc_file, fields = c("Version", "Package"))
  } else {
    stop("Folder doesn't have a package")
  }

  local_ver <- package_version(local_desc[, "Version"])

  # Remote repo
  remote <- strsplit(repo, "/", fixed = TRUE)
  if (length(remote) != 1L || lengths(remote) > 2 || grepl("@", repo, fixed = TRUE)) {
    stop("Repository ", sQuote(repo), " is not simple.\n",
         "It cannot have a branch name or a sha number", call. = FALSE)
  }
  github <- sprintf("https://raw.githubusercontent.com/%s/master/DESCRIPTION",
                    repo)

  remote_desc <- read.dcf(url(github), fields = c("Version", "Package"))
  remote_version <- package_version(remote_desc[, "Version"])

  stopifnot("Packagename does not match" = remote_desc[, "Package"] == local_desc[, "Package"])
  pkg <- local_desc[, "Package"]

  if (remote_version > local_ver) {
    stop("Update local repository ", repo, " at ", path, call. = FALSE)
  } else if (remote_version < local_ver) {
    warning("Check ", pkg, " branch at ", sQuote(path),
            ".\nIt might not the be right one,",
            immediate. = TRUE, call. = FALSE)
  } else {
    message("All good for package")
  }
  TRUE
}

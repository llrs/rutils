git_is_installed <- function() {
  nzchar(Sys.which("git"))
}


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

  if (!git_is_installed()) {
    stop("Git is not installed")
  }

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
#' @importFrom utils URLencode
#' @examples
#' \dontrun{(llrs_check_pkg_version("llrs/rutils")}
llrs_check_pkg_version <- function(repo, path = ".") {

  if (!git_is_installed()) {
    stop("Git is not installed")
  }

  # Local repo
  stopifnot("Just one path" = length(path) == 1L)
  path <- normalizePath(path)

  desc_file <- file.path(path, "DESCRIPTION")
  if (file.exists(desc_file)) {
    local_desc <- read.dcf(desc_file, fields = c("Version", "Package"))
  } else {
    stop("Folder doesn't have a package")
  }
  local_branch <- system2("git",
                         args = paste("-C", path, "rev-parse --abbrev-ref HEAD"),
                         stdout = TRUE)

  local_ver <- package_version(local_desc[, "Version"])

  # Remote repo
  remote <- strsplit(repo, "/", fixed = TRUE)
  check_repo_length <- length(remote) != 1L || lengths(remote) > 2
  if (check_repo_length || grepl("@", repo, fixed = TRUE)) {
    stop("Repository ", sQuote(repo), " is not simple.\n",
         "It cannot have a branch name or a sha number", call. = FALSE)
  }
  remotes <- system2("git", paste("-C", path, "remote -v"), stdout = TRUE)
  if (!any(grepl(pattern = repo, remotes))) {
    stop("Repository doesn't match any remote setup", call. = FALSE)
  }

  # Github redirect master to main
  # Encoding is required for @ and other reserved characters
  remote_branch <- URLencode(local_branch, reserved = TRUE)
  github <- sprintf("https://raw.githubusercontent.com/%s/%s/DESCRIPTION",
                    repo, remote_branch)

  url_github <- url(github)
  # Will break if the repository doesn't have a DESCRIPTION
  remote_desc <- tryCatch(
    suppressWarnings(read.dcf(url_github, fields = c("Version", "Package"))),
    error = function(e){NULL}
  )
  if (is.null(remote_desc)){
    warning(sprintf("Branch %s not found in remote repository %s",
                    local_branch, repo),  call. = FALSE)
    return(NA)
  }
  remote_version <- package_version(remote_desc[, "Version"])
  check_pkg_name_match <- remote_desc[, "Package"] == local_desc[, "Package"]
  stopifnot("Package name does not match" = check_pkg_name_match)
  pkg <- local_desc[, "Package"]

  if (remote_version > local_ver) {
    warning("Update local repository ", path, ", branch ", local_branch, ".",
            call. = FALSE)
    out <- FALSE
    names(out) <- pkg
    return(invisible(out))
  }
  if (remote_version < local_ver) {
    warning("Check ", pkg, " branch ", local_branch, " at ", sQuote(path),
            ".\nIt might not the be right one,",
            immediate. = TRUE, call. = FALSE)
    out <- NA
    names(out) <- pkg
    return(invisible(out))

  }
  out <- TRUE
  names(out) <- pkg
  invisible(out)
}

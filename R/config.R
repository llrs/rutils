

#' Copy config to config repository
#'
#' Copy file to config repository and add it and push it.
#'
#' If there are files with the new file and the existing one it will prompt to review them.
#' @param path Path of file to copy
#' @param dest_dir Path to folder were to save it.
#'
#' @returns TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' llrs_copy_config("~/.gitconfig")
#' }
llrs_copy_config <- function(path, dest_dir = "~/Documents/Projects/customizations") {
  if (!file.exists(path)) {
    stop("File doesn't exists")
  }

  if (!dir.exists(dest_dir)) {
    stop("Target directory doesn't exists")
  }
  old_file <- file.path(dest_dir, basename(path), fsep = "/")
  if (file.exists(old_file) && tools::Rdiff(old_file, path)) {
    warning("There are differences between the files:\n - New: ",
    path, "\n - Old: ", old_file, "\n")
    pick <- utils::askYesNo("Continue?", default = FALSE)
    if (!isTRUE(pick)) {
      return(NULL)
    }
  }
  fc <- file.copy(path, to = dest_dir, overwrite = TRUE)
  if (!fc) {
    stop("Couldn't copy file to directory.")
  }
  message("Added to the repository: ", old_file)
  system2("git", sprintf("-C %s add .", dirname(old_file)))
  system2("git", sprintf("-C %s commit -m 'Adding/updating file %s'",
                         dirname(old_file), basename(old_file)))
  system2("git", sprintf("-C %s push", dirname(old_file)))
  TRUE
}

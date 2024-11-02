#' Provide a R global user profile
#'
#' Write my preferences on a R user profile and R environment.
#' @return `TRUE` or `FALSE` depending on if it has overwritten the files or not.
#' @export
llrs_profile <- function() {
  dest_file_profile <- "~/.Rprofile"
  overwrite1 <- TRUE
  if (file.exists(dest_file_profile)) {
    overwrite1 <- askYesNo("~/.Rprofile exists. Do you want to replace it?",
             default = "Cancel")
  }
  path <- base::system.file("r_profile", package = "rutils", mustWork = TRUE)
  file.copy(path, to = dest_file_profile, overwrite = overwrite1)

  dest_file_environ <- "~/.Renviron"
  overwrite2 <- TRUE
  if (file.exists(dest_file_environ)) {
    overwrite2 <- askYesNo("~/.Renviron exists. Do you want to replace it?",
             default = "Cancel")
  }
  path <- base::system.file("r_environ", package = "rutils", mustWork = TRUE)
  file.copy(path, to = dest_file_environ, overwrite = overwrite2)

  if (overwrite1 || overwrite2) {
    if (overwrite1 && overwrite2 ) {
      msg <- "R profile and environment"
    } else if (overwrite1) {
      msg <- "R profile"
    } else if (overwrite2) {
      msg <- "R environment"
    }
    message("Adjust the ", msg, " and restart the R session.")
  }
  overwrite1 || overwrite2
}

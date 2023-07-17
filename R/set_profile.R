#' Provide a R global user profile
#'
#' Write my preferences on a R user profile.
#' @return Whatever [file.copy()] does
#' @export
#' @importFrom utils menu
llrs_profile <- function() {
  dest_file <- "~/.Rprofile"
  overwrite <- TRUE
  if (file.exists(dest_file)) {
    message("~/.Rprofile exists. Do you want to replace it?")
    overwrite <- c(TRUE, FALSE)[menu(c("Yes", "No"))]
  }
  path <- base::system.file("inst", "r_profile", package = "rutils",
                            mustWork = TRUE)
  file.copy(path, to = dest_file, overwrite = overwrite)
}

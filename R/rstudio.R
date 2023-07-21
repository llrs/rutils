# Modify/set rstudio preferences
llrs_rstudio_setup <- function() {
  check_rstudio()

  rstudioapi::readRStudioPreference("", "")
  rstudioapi::writeRStudioPreference
  r <- rstudioapi::readPreference("readRStudioPreference")

}


check_rstudio <- function() {
  if (!is.null(.state$rstudio)) {
    return(TRUE)
  }
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("Install rstudioapi", call. = FALSE)
  }

  if  (!requireNamespace("rstudioapi", quietly = TRUE) && !rstudioapi::isAvailable()) {
    stop("Not in Rstudio!", call. = FALSE)
  }

  info <- rstudioapi::versionInfo()
  if (info$version < "1.1.287") {
    warning("Some functions might not work!")
  }

  if (!requireNamespace("rappdirs", quietly = TRUE)) {
    stop("Install rappdirs", call. = FALSE)
  }



  # If user changes the Rstudio configuration folder after running this one
  # It will lead to weird behavior.
  if (is.null(.state$rstudio)) {
    rcp <- rstudio_config_path()
    .state$rstudio <- c(
      path = file.path(rcp, "rstudio-prefs.json"),
      backup =  file.path(rcp, "rstudio-prefs.json.bkp") )
  }
  TRUE
}

rstudio_config_path <- function() {
  if (.Platform$OS.type == "windows") {
    base <- rappdirs::user_config_dir("RStudio", appauthor = NULL)
  } else {
    base <- rappdirs::user_config_dir("rstudio")
  }
  base
}

# See this discussion https://github.com/ddsjoberg/rstudio.prefs/issues/18

#' Switch between Rstudio configurations.
#'
#'
#' @return Called by its side effects.
#' @rdname rstudio_config
#' @name rstudio_configuration
#' @examples
#' \dontrun{
#' llrs_rstudio_default()
#' llrs_rstudio_restore()
#' }
NULL

#' @export
#' @describeIn rstudio_config Will rename your configuration and restart RStudio,
#' to revert to a fresh Rstudio with the default configuration (useful for
#' teaching) or double checking default options.
llrs_rstudio_default <- function() {
  check_rstudio()
  rstudio_path <- .state$rstudio["path"]
  rstudio_backup <- .state$rstudio["backup"]
  if (file.exists(rstudio_backup)) {
    stop("Already a previous configuration exists.")
  }
  if (file.rename(rstudio_path, rstudio_backup)) {
    message("Restaring Rstudio")
    Sys.sleep(1)
    rstudioapi::openProject()
  }
}


#' @export
#' @export
#' @describeIn rstudio_config Will restore your configuration.
llrs_rstudio_restore <- function() {
  check_rstudio()
  check_rstudio()
  rstudio_path <- .state$rstudio["path"]
  rstudio_backup <- .state$rstudio["backup"]
  if (file.exists(rstudio_backup) && file.rename(rstudio_backup, rstudio_path)) {
    message("Restaring Rstudio")
    Sys.sleep(1)
    rstudioapi::openProject()
  } else {
    stop("Missing previous configuration")
  }
}

# Provide snippets
llrs_rstudio_snippets <- function() {

}

# Check if theme is in the computer.
llrs_rstudio_check_theme <- function(theme) {

}

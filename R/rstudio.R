# Modify/set rstudio preferences
llrs_rstudio_setup <- function() {
  check_rstudio()

  rstudioapi::readRStudioPreference("", "")
  rstudioapi::writeRStudioPreference

  r <- rstudioapi::readPreference("readRStudioPreference")

}

check_rstudio <- function() {
  if (!requireNamespace("rstudio.prefs", quietly = TRUE)) {
    stop("Install rstudio.prefs", call. = FALSE)
  }

  if  (!rstudioapi::isAvailable()) {
    stop("Not in Rstudio!", call. = FALSE)
  }
}

# See this discussion https://github.com/ddsjoberg/rstudio.prefs/issues/18
llrs_rstudio_default <- function() {
  check_rstudio()
  file <- file.path(rstudio_config_path(), "rstudio-prefs.json")
  file.rename(file, file.path(rstudio_config_path(), "rstudio-prefs.json.bkp"))
}

llrs_rstudio_restore <- function() {
  check_rstudio()
  file <- file.path(rstudio_config_path(), "rstudio-prefs.json")
}
json <- jsonlite::read_json(file)

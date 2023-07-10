#' Upload a file to Microsoft cloud in One Drive
#'
#' @param path File you want to upload
#'
#' @return The id of the file uploaded
#' @export
llrs_upload <- function(path) {
  if (!file.exists(path)) {
    stop("!File doesn't exists")
  }

  if (requireNamespace("Microsoft365R", quietly = TRUE)) {
    stop("Please install the 'Microsoft365R' package.")
  }
  business <- setup_onedrive_business()
  business$create_folder
}

setup_onedrive_business <- function() {
  if (!is.null(onedrive$business)) {
    onedrive$business <- Microsoft365R::get_business_onedrive()
  }
  onedrive$business
}

onedrive <- emptyenv()

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
llrs_download <- function(path) {
  business <- setup_onedrive_business()
  if (!nzchar(tools::file_ext(name))) {
    business$download_folder(name, recursive = TRUE, overwrite = TRUE,
                             parallel = FALSE)
  }
  business$download_file(name, overwrite = TRUE)
}

#' Title
#'
#' @param path
#' @param type
#' @param expiry
#' @param password
#' @param scope
#'
#' @return
#' @export
#'
#' @examples
llrs_share <- function(path, type, expiry, password, scope) {
  business <- setup_onedrive_business()
  business$create_share_link(name = path, type = type, expiry = expiry, scope = scope)
}

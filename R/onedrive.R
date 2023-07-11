#' Upload a file to Microsoft cloud in One Drive
#'
#'
#' Folders will be created if they don't exists.
#' @param path File you want to upload
#'
#' @return The id of the file uploaded
#' @export
llrs_upload <- function(file, dest) {
  if (!file.exists(file)) {
    stop("File doesn't exists!")
  }
  if (!nzchar(tools::file_ext(dest))) {
    stop("Destination should be the name of the file in the remote location.")
  }
  folder <- dirname(dest)
  check_onedrive()
  out <- tryCatch(.state$azure$onedrive$list_files(folder), error = function(e){
    FALSE
  }, finally = TRUE)
  if (isFALSE(out)) {
    warning("It will create a new folder!")
  }
  .state$azure$onedrive$upload_file(dest = dest, src = file)

}

check_onedrive <- function() {
  if (!requireNamespace("Microsoft365R", quietly = TRUE)) {
    stop("Please install the 'Microsoft365R' package.")
  }
  if (!requireNamespace("rappdirs", quietly = TRUE)) {
    stop("Please install the 'rappdirs' package.")
  }
  if (is.null(.state$azure$ondrive)) {
    if (!dir.exists(rappdirs::user_data_dir("AzureR"))) {
      message("R will use your current Outlook account in the default brower.")
      Sys.sleep(5)
    }
    .state$azure <- c(onedrive = Microsoft365R::get_business_onedrive())
  }
}


#' Download a file
#'
#' @param path Path to file from the One Drive.
#'
#' @return The path
#' @export
#'
llrs_download <- function(path) {
  check_onedrive()
  onedrive <- .state$azure$onedrive
  if (!nzchar(tools::file_ext(path))) {
    onedrive$download_folder(path, recursive = TRUE, overwrite = TRUE,
                             parallel = FALSE)
  }
  onedrive$download_file(path, overwrite = TRUE)
}

#' Create a link to a shared file
#'
#' Uses the credentials to share a link to a resource.
#' @param path The path to the file
#' @param type One of c("view", "edit", "embed").
#' @param expiry Time it is valid: "x days", "x hours".
#' @param password Password protected ?
#' @param scope I don't know
#' @seealso [func()]
#' @return A link to share a resource
#' @export
llrs_share <- function(path, type, expiry, password, scope) {
  check_onedrive()
  business <- .state$onedrive$business
  business$create_share_link(name = path, type = type, expiry = expiry,
                             scope = scope)
}

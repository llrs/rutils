get_onedrive <- function() {
  .state[["azure"]][["onedrive"]]
}

#' Upload a file to Microsoft cloud in One Drive
#'
#'
#' Folders will be created if they don't exists.
#' @param file File you want to upload.
#' @param dest Where you want to save the file.
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
  onedrive <- check_onedrive()
  out <- tryCatch(onedrive$list_files(folder), error = function(e) {
    FALSE
  }, finally = TRUE)
  if (isFALSE(out)) {
    warning("It will create a new folder!")
  }
  onedrive$upload_file(dest = dest, src = file)

}

check_onedrive <- function() {
  check_installed("Microsoft365R")
  check_installed("rappdirs")

  if (is.null(.state$azure$ondrive)) {
    if (!dir.exists(rappdirs::user_data_dir("AzureR"))) {
      message("R will use your current Outlook account in the default brower.")
      Sys.sleep(5L)
    }
    .state$azure <- c(onedrive = Microsoft365R::get_business_onedrive())
  }
  get_onedrive()
}


#' Download a file
#'
#' @param file Path to file from the One Drive.
#' @param dest Path to where the file should be stored.
#'
#' @return The path of the file in your computer.
#' @export
#'
llrs_download <- function(file, dest = ".") {
  onedrive <- check_onedrive()
  if (!nzchar(tools::file_ext(file))) {
    onedrive$download_folder(src = file, recursive = TRUE, overwrite = TRUE,
                             parallel = FALSE, dest = dest)
  }
  onedrive$download_file(file, overwrite = TRUE, dest = dest)
}

#' Create a link to a shared file
#'
#' Uses the credentials to share a link to a resource.
#' @param path The path to the file
#' @param type One of c("view", "edit", "embed").
#' @param expiry Time it is valid: "x days", "x hours".
#' @param password Password protected ?
#' @param scope I don't know.
#' @seealso [llrs_download()]
#' @return A link to share a resource
#' @export
llrs_share <- function(path, type, expiry, password, scope) {
  onedrive <- check_onedrive()
  onedrive$create_share_link(name = path, type = type, expiry = expiry,
                             scope = scope)
}

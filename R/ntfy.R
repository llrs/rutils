# Use ntfy to send a message

check_ntfy <- function() {
  if (!check_installed("ntfy")) {
    stop("Install 'jonocarroll/ntfy' package")
  }
}

#' Send a message to the NTFY address
#'
#' @param message Text to be sent.
#' @param title Title of the notification.
#' @param ... Other arguments used in `ntfy::ntfy_send()`
#' @param topic  Send the message to the default NTFY environment variable:
#' "NTFY_TOPIC". Specify topic if you don't use the default.
#' @export
#' @returns The default `httr` response.
#' @examples
#' \dontrun{
#' if(requireNamespace(“ntfy”)) {
#'     llrs_send_ntfy("Failure", "test")
#' }
#' }
llrs_send_ntfy <- function(message, title, ..., topic  = NULL) {
  check_ntfy()
  topic <- if (is.null(topic)) ntfy::ntfy_topic() else topic
  # server <- if (...length() && !"server" %in% ...names) ntfy::ntfy_server() else "ntfy.sh"
  ntfy::ntfy_send(message, title, topic = topic, ...)
}

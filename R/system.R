
#' Use system's notify-send to send a message
#'
#' Creates a pop up message on the system if it is run from an interactive
#' session.
#' @param ... Ignored arguments (to make it compatible with a pipe)
#' @param title Title of the message: character shorter than 40 characters.
#' @param message Message of the notification.
#' @param icon Icon, to pick from the system.
#' @param urgency Urgency of the system
#'
#' @return Always TRUE (even if no notification can be created).
#' @export
#' @examples
#' notify_system(title = "Works", message = "My first message")
notify_system <- function(..., title, message = NULL, icon = NULL, urgency = NULL) {
  match.arg(urgency, c("low", "normal", "critical"))
  if (!is.null(icon)) {
    icon <- sprintf("-i %s", icon)
  }
  if (!is.null(icon)) {
    urgency <- sprintf("-u %s", urgency)
  }
  if (nchar(title) > 40) {
    warning("title might be too big", call. = FALSE)
  }
  if (interactive()) {
    system2("notify-send", args = paste(icon, urgency, sQuote(title,q = '"'),
                                        sQuote(message, q = '"')))
  }
  TRUE
}

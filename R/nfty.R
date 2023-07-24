# Use nfty to send a message

check_nfty <- function() {
  if (!requireNamespace("ntfy", quietly = TRUE)) {
    stop("Install 'jonocarroll/ntfy' package")
  }
}

llrs_send_nfty <- function(message, title, ..., topic  = NULL) {
  check_nfty()
  topic <- if (is.null(topic)) ntfy::ntfy_topic() else topic
  ntfy::ntfy_send(message, title, topic = topic, ...)
}

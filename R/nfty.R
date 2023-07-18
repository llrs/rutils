# Use nfty to send a message

check_nfty <- function() {
  if (!requireNamespace("ntfy", quietly = TRUE)) {
    stop("Install 'jonocarroll/ntfy' package")
  }
}

llrs_send_nfty <- function(message, title, topic) {
  check_nfty()
  ntfy::ntfy_send(message, title, topic)
}

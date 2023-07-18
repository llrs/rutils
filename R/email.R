# Send emails

get_azure <- function() {
  .state$azure$outlook
}

check_email <- function() {
  if (!requireNamespace("emayili", quietly = TRUE) ||
      !requireNamespace("blastula", quietly = TRUE)) {
    stop("Install either 'emayili' or 'blastula'")
  }
  if (!requireNamespace("Microsoft365R", quietly = TRUE)) {
    stop("Please install the 'Microsoft365R' package.")
  }
  if (!requireNamespace("rappdirs", quietly = TRUE)) {
    stop("Please install the 'rappdirs' package.")
  }
  if (is.null(get_azure())) {
    if (!dir.exists(rappdirs::user_data_dir("AzureR"))) {
      message("R will use your current Outlook account in the default brower.")
      Sys.sleep(5)
    }
    suppressMessages(.state$azure <- c(outlook = Microsoft365R::get_business_outlook()))
  }
  get_azure()
}

check_attachments <- function(attachments) {
  if (is.null(attachments)) {
    return(TRUE)
  }

  file_exists <- vapply(attachments, file.exists, FUN.VALUE = logical(1))
  if (!all(file_exists)) {
    missing <- attachments[!file_exists]
    stop("Not able to find file(s): ", dQuote(missing, q = '"'))
  }
}

check_to <- function(email_address) {
  if (!all(grepl("@", email_address, fixed = TRUE))) {
    stop("Not a valid email", call. = FALSE)
  }
  if (startsWith(email_address, "lrevilla")) {
    stop("Do not sent emails to yourself!")
  }
  TRUE
}

choose_email_method <- function(attachments = NULL) {
  possibilities <- c("microsoft" = TRUE,
                     "emayili" = requireNamespace("emayili", quietly = TRUE),
                     "blastula" = requireNamespace("blastula", quietly = TRUE)
                     )
  out <- names(possibilities)[possibilities]
  if (length(out) > 1) {
    out <- out[1]
  }
  out
}


#' Send a quick email
#'
#' @param text Text of the email?
#' @param subject What is the subject of the email?
#' @param to Address to send the emails (no CC or BCC)
#' @param attachments Paths to files.
#'
#' @return Called by its side effects
#' @export
llrs_send_email <- function(text, subject, to, attachments = NULL) {
  check_attachments(attachments)
  check_to(to)
  outlook <- check_email()
  method <- choose_email_method(attachments)


  if (method == "emayili") {
    ey_email <- emayili::envelope(
      text = text, to = to, subject = subject) |>
      emayili::attachment(attachments)
    em <- outlook$create_email(ey_email)
  }

  if (method == "blastula") {
    bl_em <- blastula::compose_email(
      body = blastula::md(body)) |>
      blastula::add_attachment(attachments)
    em <- outlook$create_email(bl_em, subject = subject, to = to)
  }
  if (method == "microsoft") {
    em <- outlook$create_email(text, content_type = "html",to = to, subject = subject,
                         send_now = FALSE)
    if (!is.null(attachments)) {
      em$add_attachment(attachments)
    }
  }
  em$send()
}

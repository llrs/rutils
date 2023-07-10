# Send emails
#
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
  if (is.null(.state$azure$ondrive)) {
    if (!dir.exists(rappdirs::user_data_dir("AzureR"))) {
      message("R will use your current Outlook account in the default brower.")
      Sys.sleep(5)
    }
    .state$azure <- c(outlook = Microsoft365R::get_business_outlook())
  }
}

check_email_args <- function() {
  # whatever
}

choose_email_method <- function() {
  possibilities <- c("microsoft" = TRUE,
                     "blastula" = requireNamespace("blastula", quietly = TRUE),
                     "emayili" = requireNamespace("emayili", quietly = TRUE))
  out <- names(possibilities)[possibilities]
  if (length(out) > 1) {
    # Whatever
  }
  out
}


#' Send a quick email
#'
#'
#' @return Called by its side effects
#' @export
llrs_send_email <- function() {
  check_email()
  outlook <- .state$azure$outlook
  check_email_args()
  method <- choose_email_method()


  if (method == "emayili") {
    ey_email <- emayili::envelope(
      text = "Hello from emayili",
      to = "user@example.com",
      subject = "example emayili email") |>
      emayili::attachment("mydocument.docx") |>
      emayili::attachment("mydata.xlsx")
    outlook$create_email(ey_email)
  }

  if (method == "blsatula") {


  }

  outlook$create_email("<emph>Emphatic hello</emph> from R",
                       content_type = "html",
                       to = "user@example.com",
                       subject = "example email",
                       send_now = FALSE)$
    set_subject("example email")
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ntfy.R
\name{llrs_send_ntfy}
\alias{llrs_send_ntfy}
\title{Send a message to the NTFY address}
\usage{
llrs_send_ntfy(message, title, ..., topic = NULL)
}
\arguments{
\item{message}{Text to be sent.}

\item{title}{Title of the notification.}

\item{...}{Other arguments used in \code{ntfy::ntfy_send()}}

\item{topic}{Send the message to the default NTFY environment variable:
"NTFY_TOPIC". Specify topic if you don't use the default.}
}
\value{
The default \code{httr} response.
}
\description{
Send a message to the NTFY address
}
\examples{
\dontrun{
if(requireNamespace(“ntfy”)) {
    llrs_send_ntfy("Failure", "test")
}
}
}

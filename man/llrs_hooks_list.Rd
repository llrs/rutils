% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/git_hooks.R
\name{llrs_hooks_list}
\alias{llrs_hooks_list}
\title{List existing hooks}
\usage{
llrs_hooks_list(full.names = FALSE)
}
\arguments{
\item{full.names}{A logical value to be passed to list.files}
}
\value{
A character vector with the name of the hooks.
}
\description{
Check which hooks are in the package.
}
\examples{
llrs_hooks_list()
}
\seealso{
\code{\link[=llrs_hook]{llrs_hook()}}
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/git.R
\name{llrs_check_pkg_version}
\alias{llrs_check_pkg_version}
\title{Check package version}
\usage{
llrs_check_pkg_version(repo, path = ".")
}
\arguments{
\item{repo}{Github repository.}

\item{path}{Local path to the repository.}
}
\value{
TRUE if successfull
}
\description{
Check package version
}
\examples{
\dontrun{(llrs_check_pkg_version("llrs/rutils")}
}

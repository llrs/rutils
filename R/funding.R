#' Funding
#' 
#' Set github file for funding.
#' @param path Path to project/package folder.
#' @returns Called by its side effects of creating a file.
#' @examples
#' use_funding()
use_funding <- function(path = ".") {
    funding_path <- file.path(path, ".github", "funding.yaml")
    format <- c("buy_me_a_coffee: llrs")
    cat(format, file = funding_path)
}
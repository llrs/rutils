#' Funding
#' 
#' Set github file for funding.
#' @param path Path to project/package folder.
#' @param ... Other named strings that are [accepted by GitHub](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/customizing-your-repository/displaying-a-sponsor-button-in-your-repository)
#' @returns Called by its side effects of creating a file.
#' @export
#' @examples
#' llrs_funding(path = tempdir())
llrs_funding <- function(path = ".", ...) {
    funding_path <- file.path(path, ".github", "funding.yaml")
    if (!dir.exists(dirname(funding_path))) {
        dir.create(dirname(funding_path), recursive = TRUE)
    }
    format <- c("buy_me_a_coffee: llrs")
    other <- list(...)
    if (length(other)) {
        other_formatted <- paste0(names(other), ": ", other, collapse = "\n")
        out <- paste(format, other_formatted, sep = "\n")
    } else {
        out <- format
    }
    cat(out, file = funding_path)
}
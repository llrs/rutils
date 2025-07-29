#' Send toot
#'
#' @param x A vector of strings if possible they will be joined
#' @param width Allowed width on the server
#' @param join_text Character used to join the text
#'
#'
#' @returns The ids of the toots posted
#' @export
#'
#' @examples
#' \dontrun{
#' if(requireNamespace(“rtoot”)) {
#'     llrs_send_toot(c("First message", "Second message"))
#' }
#' }
llrs_send_toot <- function(x, width = 500, join_text = ". ") {
  check_installed("rtoot")
  stopifnot(is.character(x))
  x <- x[nzchar(x) & !is.na(x)]

  len <- mast_length(x)

  if (any(len > width)) {
    w <- which(len > width)

    xw <- strsplit(x[w], split = "[.})]\\s+")
    y <- x[-w]
    for (p in seq_along(w)) {
      y <- append(y, xw[[p]], after = w[p] - 1)
    }
    x <- y
  }



  joined_length <- function(len, len_join = nchar(join_text)) {
    sum(len) + len_join*(length(len) - 1L)
  }
  for  (i in seq_along(x)) {
    jl <- joined_length(mast_length(x[1:i]))
    selected2join <- jl <= width
    # No two strings are acceptable
    if (sum(selected2join) < 2L) {
      next
    }
    repeats <- rle(selected2join)
    # There are at least two consecutive strings
    if (!any(repeats$lenghts[repeats$values] >= 2L)) {
      next
    }
    # Omit non-consecutive strings
    (which(selected2join) %in% which(selected2join) - 1L)

    # Join strings
    start_position <- which.min(!selected2join) - 1L
    x <- append(x[!selected2join],
                paste0(x[selected2join], collapse = join_text),
                after = start_position)
    x <- c(paste(x[selected2join], collapse = join_text), x[!selected2join])
  }
  # resp <- rtoot::post_thread(x)
  x
}


mast_length <- function(x) {
  len <- nchar(x)
  with_url <- grepl("https?://", x)
  if (any(with_url)) {
    g <- gregexpr("(https?://[^[:space:]]+)", x)
    g <- lapply(g, function(x){if (length(x) == 1L && x < 0) {
      return(NULL)
    } else{x}})
    urls_length <- vapply(g, function(i){sum(attr(i, "match.length"))}, numeric(1L))
    urls_length[urls_length < 0] <- 0

    len - urls_length + lengths(g)*25
  } else {
    len
  }
}

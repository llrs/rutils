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
  if (length(x) == 1L) {
    return(x)
  }
  x <- x[nzchar(x) & !is.na(x)]

  len <- mast_length(x)

  # Split long message into multiple strings to later combine them iff needed.
  if (any(len > width)) {
    w <- which(len > width)

    xw <- strsplit(x[w], split = "[.})]\\s+")
    y <- x[-w]
    for (p in seq_along(w)) {
      y <- append(y, xw[[p]], after = w[p] - 1)
    }
    x <- y
  }

  join_messages(x = x, width = width, join_text = join_text)
}


join_messages <- function(x, width, join_text) {
  len <- mast_length(x)
  names(len) <- NULL
  length_text_joiner <- c(0, rep_len(1, length(x) - 1)) * nchar(join_text)
  below_limits <- len + length_text_joiner < width

  # No two text below the limits
  if (sum(below_limits) < 2L) {
    return(x)
  }

  # All strings are below the limit: one message as output
  all_below_limits <- sum(len + length_text_joiner) <= width
  if (all(below_limits) && all_below_limits) {
    return(paste0(x, collapse = join_text))
  }

  wbl <- which(below_limits)

  # Consecutive messages to join
  # consecutive (cons) indexes (i)
  cons_i <- intersect(wbl + 1, wbl)
  cons_i <- c(cons_i - 1, max(cons_i))

  # No consecutive messages to join
  if (!length(cons_i)) {
    return(x)
  }
  cons_i <- sort(cons_i)

  # Find start and end of consecutive (cons) indexes (i)
  start_cons <- c(0, cons_i[-length(cons_i)]) == cons_i
  starts_joins <- cons_i[which(start_cons)]

  ends_cons <- c(which(start_cons) - 1, length(start_cons))
  ends_joins <- cons_i[ends_cons]


  joined_text <- Map(function(start, end) {
    paste0(x[seq(from = start, to = end)], collapse = join_text)
  }, start = starts_joins, end = ends_joins)

  # Replace joined text by new text
  no_starts <- setdiff(cons_i, start_cons)
  x <- x[-no_starts]
  x[start_cons] <- unlist(joined_text, recursive = FALSE, use.names = FALSE)
  x
}

find_urls <- function(x) {
  htps <- gregexpr("(https?://[^[:space:]]+)", x)
  l <- lapply(htps, function(x){if (length(x) == 1L && x < 0) {
    return(NULL)
  } else{x}})
  names(l) <- seq_len(length(x))
  l[lengths(l) >= 1L]
}

urls_info <- function(urls) {
  if (length(urls) == 0L) {
    return(NA)
  }
  l <- lapply(urls, function(u) {
    length <- attr(u, "match.length")
    attributes(u) <- NULL
    data.frame(Position = u, Length = length)
  })
  names_l <- rep(names(l), vapply(l, NROW, FUN.VALUE = numeric(1L)))
  out <- do.call(rbind, l)
  out <- cbind(Message = names_l, out)
  rownames(out) <- NULL
  out
}

substract_len <- function(len, ui) {
  diff_length <- vapply(split(ui, ui$Message),
                        function(x) {
                          # Each url counts per 25 characters
                          NROW(x)*25 - sum(x$Length)},
                        numeric(1L))
  names(len) <- as.character(seq_along(len))
  len[names(diff_length)] <- len[names(diff_length)] - diff_length
  len
}

mast_length <- function(x) {
  len <- nchar(x)
  with_url <- grepl("https?://", x)
  if (any(with_url)) {
    g <- find_urls(x)
    ui <- urls_info(g)
    substract_len(len, ui)
  } else {
    len
  }
}

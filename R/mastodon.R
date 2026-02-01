#' Send toot
#'
#' Split and merge the text as needed to reduce the number of toots to post.
#' It only handles text and not images as per `rtoot::post_thread()`
#'
#' @param msg A vector of strings if possible they will be joined
#' @param width Allowed width on the server
#' @param join_text Character used to join the text
#' @returns The ids of the toots posted
#' @export
#'
#' @examples
#' \dontrun{
#' if(requireNamespace(“rtoot”)) {
#'     llrs_toots(c("First message", "Second message", "Testing rtoo::post_threads"))
#' }
#' }
llrs_toots <- function(msg, width = 500, join_text = ". ") {
  check_installed("rtoot")
  stopifnot(is.character(msg))
  if (length(msg) == 1L) {
    return(msg)
  }
  msg <- msg[nzchar(msg) & !is.na(msg)]

  msg_split <- split_messages(msg = msg, width = width)
  msg_joined <- join_messages(msg = msg_split, width = width, join_text = join_text)
  rtoot::post_thread(msg_joined, language = "en")
}

split_messages <- function(msg, width, len = mast_length(msg)) {
  # Split long message into multiple strings to later combine them iff needed.
  msg2split <- which(len > width)
  if (!length(msg2split)) {
    return(msg)
  }

  msg_split <- strsplit(msg[msg2split], split = "[.})]\\s+")
  if (any(mast_length(unlist(msg_split, FALSE, FALSE)) > width)) {
    stop("Couldn't split the messages into smaller sentences.")
  }

  msg_wo_split <- msg[-msg2split]
  # Get the text back to its place
  for (p in seq_along(msg2split)) {
    msg_wo_split <- append(msg_wo_split, msg_split[[p]], after = msg2split[p] - 1L)
  }
  msg_wo_split
}

join_messages <- function(msg, width, join_text) {

  njoin <- mast_length(join_text)
  p <- 1
  repeat {
    index <- c(p, p + 1)
    len <- mast_length(msg[index])
    if (sum(len) + njoin  < width) {
      msg[p] <- paste0(msg[index], collapse = join_text)
      msg <- msg[-index[2]]
    } else {
      p <- p + 1L
    }

    if (p == length(msg)) {
      break
    }
  }
  msg
}

find_urls <- function(msg) {
  htps <- gregexpr("(https?://[^[:space:]]+)", msg)
  l <- lapply(htps, function(msg){if (length(msg) == 1L && msg < 0) {
    return(NULL)
  } else{msg}})
  names(l) <- seq_len(length(msg))
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
  function(msg) {
    # Each url counts per 25 characters
    NROW(msg)*25 - sum(msg$Length)},
    numeric(1L))
    names(len) <- as.character(seq_along(len))
    len[names(diff_length)] <- len[names(diff_length)] - diff_length
    len
  }

  mast_length <- function(msg) {
    len <- nchar(msg)
    with_url <- grepl("https?://", msg)
    if (any(with_url)) {
      g <- find_urls(msg)
      ui <- urls_info(g)
      substract_len(len, ui)
    } else {
      len
    }
  }

#' Check dataset
#'
#' Not tested!
#' Checks if the visits of a cohort are consistently.
#' That samples have enough/all the visits.
#' @param x A data.frame with information about the cohort.
#' @param timeline A data.frame with three columns, the visit name, the time since the first visit and a category.
#' @param participant_col The name of the column holding the identification of the participant.
#' @param timediff The name of the column with the time difference between visits in `timeline`.
#' @param time The name of the column with the visit time in `x`.
#' @details
#' Assumes that x and timelines have the same column names for sample id and visit timepoints.
#' In addition, there should be a third column in timeline about the
#'
#' @return
#' @export
check_datasets <- function(x, timeline, participant_col, timediff, time) {
  stopifnot(ncol(timeline) != 3)
  stopifnot(length(intersect(colnames(x), colnames(timeline))) == 2L)
  stopifnot(timediff %in% colnames(timeline))
  stopifnot(time %in% colnames(x))

  out <- merge(x, timeline, suffixes = c(".dataset", ".timeline"),
        all.x = TRUE, all.y = FALSE,
        sort = FALSE)

  # Check visits per participant
  df <- as.data.frame(table(out[[participant_col]], out[[time]]))
  colnames(df) <- c("participant_id", "visit", "n")
  if (any(df[["n"]] < NROW(timeline))) {
    warning("Some participants miss a visit.", call. = TRUE)
  }
  if (any(df[["n"]] > NROW(timeline))) {
    warning("Some participants have more visits than reported.", call. = TRUE)
  }

  # Check visits time diff
  df <- as.data.frame(table(out[[time]], out[[timediff]]))
  colnames(df) <- c("time", "timediff", "n")
  if (any(df[["n"]] < NROW(timeline))) {
    warning("Some participants miss a visit.", call. = TRUE)
  }
  if (any(df[["n"]] > NROW(timeline))) {
    warning("Some participants have more visits than reported.", call. = TRUE)
  }



}

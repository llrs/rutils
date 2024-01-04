#' Create a sample sheet from files
#'
#' Create a sample sheet from fastq.gz files for easier creation of sample
#' sheets for several programs (multiqc, fastqc, ...).
#'
#' Assumes a paired end sequencing.
#' @param path A path to a folder containing the files.
#' @returns A data.frame with the names of the files.
#' @references [CNAG](https://www.cnag.eu/)
#' @export
#' @examples
#' # llrs_cnag_samples("AUSER_01.xls")
llrs_cnag_samples <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  lf <- list.files(path,
                   recursive = TRUE,
                   full.names = TRUE,
                   pattern = "\\.fastq.gz$")
  if (!(length(lf) %% 2)) {
    stop("Missing files or not paired end.")
  }
  # Assume that the paired files are in the same folder
  lf_ord <- sort(lf)
  df <- data.frame(d1 = lf_ord[endsWith(lf, "_1.fastq.gz")],
             d2 = lf_ord[endsWith(lf, "_2.fastq.gz")])
  # Check that they are properly paired.
  all(gsub("_1.fastq.gz$", "", df$d1) == gsub("_2.fastq.gz$", "", df$d2))
  df
}


#' Read the CNAG stats
#'
#' @param path Path to excel file.
#'
#' @return A data.frame with the data.
#' @references [CNAG](https://www.cnag.eu/)
#' @export
#' @examples
#' # llrs_cnag_stats("AUSER_01_Sample_Stats.xls")
llrs_cnag_stats <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Install readxl", call. = FALSE)
  }

  path <- normalizePath(path)
  if (!file.exists(path)) {
    stop("This file doesn't exists")
  }
  r <- readxl::read_excel(path, guess_max = 20,
                          .name_repair = "check_unique")
  as.data.frame(r)
}

#' Read the delivery file
#'
#' Import the delivery file which has information about the machine, application
#' and samples provided by the researcher.
#'
#' @param path Path to the project file.
#' @references [CNAG](https://www.cnag.eu/)
#' @examples
#' # llrs_cnag_deliver("AUSER_01.xls")
llrs_cnag_deliver <- function(path) {

  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Install readxl", call. = FALSE)
  }
  path <- normalizePath(path)
  if (!file.exists(path)) {
    stop("This file doesn't exists")
  }
  r <- readxl::read_excel(path, skip = 2,
                          guess_max = 20, .name_repair = "check_unique")
  # <flowcell>_<lane>_<index>_<read>.fastq.gz
  # Assumes that the three first columns are those
  stopifnot(tolower(colnames(r)[1:3]) == c("flowcell", "lane", "multiplex index"))
  root <- apply(r[, 1:3], 1, paste0, collapse = "_")
  r$d1 <- paste0(root, "_1.fastq.gz")
  r$d2 <- paste0(root, "_2.fastq.gz")
  as.data.frame(r)
}

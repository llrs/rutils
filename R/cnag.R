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
  if (length(lf) %% 2) {
    stop("Missing files or not paired end.")
  }
  # Assume that the paired files are in the same folder
  lf_ord <- sort(lf)
  df <- data.frame(f1 = lf_ord[endsWith(lf, "_1.fastq.gz")],
             f2 = lf_ord[endsWith(lf, "_2.fastq.gz")])
  # Check that they are properly paired.
  all(gsub("_1.fastq.gz$", "", df$d1) == gsub("_2.fastq.gz$", "", df$d2))
  df$d1 <- basename(df$f1)
  df$d2 <- basename(df$f2)
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
  if (grepl("[0-9]{2}.xls$", x = path)) {
    warning("This might file as this file might require `llrs_cnag_stats()`", call. = FALSE)
  }
  r <- readxl::read_excel(path, guess_max = 20, na = c("", "NA"),
                          .name_repair = "check_unique")
  rdf <- as.data.frame(r)
  first_empty <- min(which(is.na(rdf[, 1])))
  rdf[-seq(from = first_empty, to = NROW(rdf)), ]
}

#' Read the delivery file
#'
#' Import the delivery file which has information about the machine, application
#' and samples provided by the researcher.
#'
#' @param path Path to the project file.
#' @references [CNAG](https://www.cnag.eu/)
#' @export
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
  if (endsWith(path, "_Sample_Stats.xls")) {
    warning("This might file as this file might require `llrs_cnag_stats()`", call. = FALSE)
  }
  r <- readxl::read_excel(path, skip = 2, na = c("", "NA"),
                          guess_max = 20, .name_repair = "check_unique")
  # <flowcell>_<lane>_<index>_<read>.fastq.gz
  # Assumes that the three first columns are those
  stopifnot(tolower(colnames(r)[1:3]) == c("flowcell", "lane", "multiplex index"))
  root <- apply(r[, 1:3], 1, paste0, collapse = "_")
  r$d1 <- paste0(root, "_1.fastq.gz")
  r$d2 <- paste0(root, "_2.fastq.gz")
  as.data.frame(r)
}


#' Prepare CNAG data for cellranger
#'
#' CNAG returns the data in a format that is not compatible with cellranger.
#' @param path Path to a file with information about the delivery file.
#' @return A data.frame with the output of the names of the files ("d1" and "d2")
#'  and the new names c("cr1", "cr2")they should have.
#' @export
#' @references <https://www.10xgenomics.com/support/software/cell-ranger/latest/analysis/inputs/cr-specifying-fastqs#file-naming-convention>
#' @examples
#' # out <- llrs_cnag_cellranger("PROJECT_01.xls")
#' # out[, c("SAMPLE NAME", "d1", "cr1", "d1", "cr2")]
llrs_cnag_cellranger <- function(path){
  d <- llrs_cnag_deliver(path)
  d$cr1 <- paste0(d$`SAMPLE NAME`, "_S1_L00", d$LANE, "_R1_001.fastq.gz")
  d$cr2 <- paste0(d$`SAMPLE NAME`, "_S1_L00", d$LANE, "_R2_001.fastq.gz")
  d
}

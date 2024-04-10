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
#' # llrs_cnag_samples("AUSER_01")
llrs_cnag_samples <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  project <- basename(path)
  if (!file.info(path)$isdir) {
    stop("Expected path to project folder not to a file.")
  }

  lf <- list.files(path,
                   recursive = TRUE,
                   full.names = TRUE,
                   pattern = "\\.fastq.gz$")
  if (length(lf) %% 2) {
    stop("Missing files or not paired end.")
  }
  # Assume that the paired files are in the same folder
  lf_ord <- sort(lf)
  df <- data.frame(project = project,
                   f1 = lf_ord[endsWith(lf, "_1.fastq.gz")],
                   f2 = lf_ord[endsWith(lf, "_2.fastq.gz")])
  # Check that they are properly paired.
  all(gsub("_1.fastq.gz$", "", df$d1) == gsub("_2.fastq.gz$", "", df$d2))
  df$d1 <- basename(df$f1)
  df$d2 <- basename(df$f2)
  df
}


#' Read the CNAG stats
#'
#' Read the stats of the samples processed by CNAG.
#' @param path Path to excel file.
#' @return A data.frame with the data and the name of the project from the file name.
#' @references [CNAG](https://www.cnag.eu/)
#' @export
#' @examples
#' # llrs_cnag_stats("AUSER_01_Sample_Stats.xls")
llrs_cnag_stats <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Install readxl", call. = FALSE)
  }

  path <- normalizePath(path, mustWork = TRUE)
  project <- tools::file_path_sans_ext(basename(path))
  project <- gsub("_Sample_Stats", "", project)
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
  out <- rdf[-seq(from = first_empty, to = NROW(rdf)), ]
  cbind(project = project, out)
}

#' Read the delivery file
#'
#' Import the delivery file which has information about the machine, application
#' and samples provided by the researcher.
#'
#' @param path Path to the project file.
#' @references [CNAG](https://www.cnag.eu/)
#' @return A data.frame with the information in the project and its name and the name of the fastq files of the project.
#' @export
#' @examples
#' # llrs_cnag_deliver("AUSER_01.xls")
llrs_cnag_deliver <- function(path) {

  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Install readxl", call. = FALSE)
  }
  path <- normalizePath(path, mustWork = TRUE)

  project <- tools::file_path_sans_ext(basename(path))
  if (endsWith(path, "_Sample_Stats.xls")) {
    warning("This might file as this file might require `llrs_cnag_stats()`", call. = FALSE)
  }
  r <- readxl::read_excel(path, skip = 2, na = c("", "NA"),
                          guess_max = 20, .name_repair = "check_unique")
  # <flowcell>_<lane>_<index>_<read>.fastq.gz
  # Assumes that the three first columns are those
  stopifnot(tolower(colnames(r)[1:3]) == c("flowcell", "lane", "multiplex index"))
  root <- apply(r[, 1:3], 1, paste0, collapse = "_")
  out <- cbind(r, project = project)
  out$d1 <- paste0(root, "_1.fastq.gz")
  out$d2 <- paste0(root, "_2.fastq.gz")
  as.data.frame(out)
}


#' Prepare CNAG data for cellranger
#'
#' CNAG returns the data in a format that is not compatible with cellranger.
#' This function creates symlinks to files from the project.
#' @param path Path to a file with information about the delivery file.
#' @param out_dir Path to a folder where the files should be placed.
#' If `NULL` it doesn't create the symlinks to the folder.
#' @return A data.frame with the output of the names of the files ("d1" and "d2")
#'  and the new names c("cr1", "cr2")they should have.
#'  Also called by its side effect of creating a symlink to the original files.
#' @details
#' The CNAG project is assumed to be below the file in `path` with all the fastq files.
#'
#' @export
#' @references <https://www.10xgenomics.com/support/software/cell-ranger/latest/analysis/inputs/cr-specifying-fastqs#file-naming-convention>
#' @examples
#' # out <- llrs_cnag_cellranger("PROJECT_01.xls")
#' # out[, c("SAMPLE NAME", "d1", "cr1", "d1", "cr2")]
llrs_cnag_cellranger <- function(path, out_dir) {

  d <- llrs_cnag_deliver(path)
  lf <- list.files(dirname(path), pattern = ".*\\.fastq(\\.gz)?$",
                   recursive = TRUE,
                   all.files = TRUE, full.names = TRUE)

  if (length(lf) == 0L) {
    stop("No fastq files found below the path: ", path)
  }

  # Work with paths
  full_dir <- normalizePath(dirname(lf), mustWork = TRUE)
  # Allow NULL path
  if (!is.null(out_dir)) {
    od <- normalizePath(out_dir)
  } else {
    od <- "."
  }

  if (!dir.exists(od)) {
    dir.create(od, recursive = TRUE)
  }

  # Match files and samples
  m <- match(basename(lf), d$d1)
  m2 <- match(basename(lf), d$d2)
  full_dir <- dirname(lf)[m[!is.na(m)]]

  if (nrow(d) *2 != length(lf)) {
    stop("Not all fastq files are present!")
  }
  # Rewrite the files paths
  d$d1 <- file.path(full_dir,
                    d$d1)
  d$d2 <- file.path(full_dir,
                    d$d2)
  # The samples are concatenated with the index, the flowcell and the lane.
  # This is to account when samples/libraries are sequenced multiple times...
  # It could create a problem when a code is repeated...
  d$cr1 <- file.path(od,
                     paste0(d$`MULTIPLEX INDEX`, "-", d$FLOWCELL, "_L00",
                            d$LANE, "_R1_001.fastq.gz"))
  d$cr2 <- file.path(od,
                     paste0(d$`MULTIPLEX INDEX`, "-", d$FLOWCELL, "_L00",
                            d$LANE, "_R2_001.fastq.gz"))

  if (length(unique(d$cr1)) != nrow(d)) {
    stop("The number of unique files and the number of original files do not match!
         This requires change on the code.")
  }

  # Create symlinks
  if (!is.null(out_dir)) {
    warning("Creating symlinks")
    file.symlink(d$d1, d$cr1)
    file.symlink(d$d2, d$cr2)
  }
  # Return the information
  d
}

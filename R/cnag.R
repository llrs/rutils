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
  if (!is_dir(path)) {
    stop("Expected path to project folder not to a file.", call. = FALSE)
  }

  lf <- list.files(path,
                   recursive = TRUE,
                   full.names = TRUE,
                   pattern = "\\.fastq.gz$")
  if (length(lf) %% 2) {
    stop("Missing files or not paired end.", call. = FALSE)
  }
  # Assume that the paired files are in the same folder
  lf_ord <- sort(lf)
  df <- data.frame(project = project,
                   f1 = lf_ord[endsWith(lf, "_1.fastq.gz")],
                   f2 = lf_ord[endsWith(lf, "_2.fastq.gz")])
  # Check that they are properly paired.
  all(gsub("_1.fastq.gz$", "", df$f1) == gsub("_2.fastq.gz$", "", df$f2))
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
    stop("This file doesn't exists", call. = FALSE)
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
#' This function merges the data of CNAG to be able to prepare for it.
#' @param path Path to a file with information about the delivery file.
#' @return A data.frame with the output of the names of the files ("d1" and "d2")
#'  and its associated information.
#' @details
#' The CNAG project is assumed to be below the file in `path` with all the fastq files.
#' @export
#' @examples
#' # out <- llrs_cnag_cellranger("PROJECT_01.xls")
#' # out[, c("SAMPLE NAME", "d1", "cr1", "d1", "cr2")]
llrs_cnag_files <- function(path) {
  deliver <- llrs_cnag_deliver(path)
  files <- llrs_cnag_samples(dirname(path))
  merge(deliver, files, sort = FALSE, all = TRUE)
}


#' Create symlinks
#'
#' Create symlinks for cellranger format (see references).
#' @param x A data.frame with the information.
#' At least `c("LANE", "MULTIPLEX INDEX", "FLOWCELL", "d1", "d2")` columns must
#' be present.
#' @param name The name of the sample, it can be a single character with the
#' name of the column or a vector of characters with the sample name for each file.
#' @param out_dir Path to a folder where the files should be placed.
#' If `NULL` it doesn't create the symlinks to the folder.
#' @return Called by its side effect but also returns a data.frame.
#' @references <https://www.10xgenomics.com/support/software/cell-ranger/latest/analysis/inputs/cr-specifying-fastqs#file-naming-convention>
#' @export
llrs_cnag_symlinks <- function(x, name, out_dir) {
  if (!is.data.frame(x) && !NROW(x) > 2) {
    stop("Requiring at least two files per sample in a data.frame!", call. = FALSE)
  }
  columns <- c("LANE", "MULTIPLEX INDEX", "FLOWCELL", "f1", "f2")
  if (is.character(name) && length(name) == 1) {
    columns <- c(columns, name)
  }

  if (!any(columns %in% colnames(x))) {
    stop("Missing columns. It should have: ",
         paste(columns, collapse = ", "),
         call. = FALSE)
  }

  if (!is.character(name) || length(name) == NROW(x)) {
    stop("If name isn't character of the same length as the files", call. = FALSE)
  }

  if (length(x) > 1) {
    x$name <- name
    name <- "name"
  }
  # NULL path or empty character
  create_symlinks <- !is.null(out_dir) && length(out_dir) == 1
  if (length(out_dir) > 1) {
    stop("Please use just one path", call. = FALSE)
  }
  if (create_symlinks) {
    od <- normalizePath(out_dir)
  } else {
    od <- "."
  }
  # Following the recommendation of having subfolders for flowcell
  # <https://www.10xgenomics.com/support/software/cell-ranger/latest/advanced/cr-multi-config-csv-opts#libraries>
  od <- file.path(od, x[[name]], x$FLOWCELL)
  if (create_symlinks && any(!dir.exists(od))) {
    s <- sapply(od, dir.create, recursive = TRUE, mode = "0744")
  }
  if (length(unique(x$cr1)) != nrow(x)) {
    stop("The number of unique files and the number of original files do not match!
         This requires change on the code.", call. = FALSE)
  }
  # The samples are concatenated with the index, the flowcell and the lane.
  # This is to account when samples/libraries are sequenced multiple times...
  # It could create a problem when a code is repeated...
  x$cr1 <- file.path(od,
                     paste0(x[[name]], "_S1_L00",
                            x$LANE, "_R1_001.fastq.gz"))
  x$cr2 <- file.path(od,
                     paste0(x[[name]], "_S1_L00",
                            x$LANE, "_R2_001.fastq.gz"))

  # Create symlinks
  if (create_symlinks) {
    warning("Creating symlinks", call. = FALSE)
    file.symlink(d$f1, d$cr1)
    file.symlink(d$f2, d$cr2)
  }
}

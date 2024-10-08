
#' @name Bioinformatics
#' @title Bioinformatics
#' @description
#' Some scripts are present at [llrs/aligning](https://github.com/llrs/aligning/).
#'
#' See the functions from `llrs_cnag` among others.
NULL


#' Create positions of a box
#'
#' Create a data.frame with the positions of the sample in a square box
#' @param size A numeric value with the number of rows or columns.
#'
#' @return A data.frame with the rows and columns.
#' @export
#' @examples
#' box10 <- llrs_box(10)
#' box9 <- llrs_box(9)
llrs_box <- function(size){
  stopifnot(is.numeric(size) && size %% 1 == 0L)
  sl <- seq_len(size)
  data.frame(fila =  rep(LETTERS[sl], each = size), columna = rep(sl))
}


#' Clean cellranger multi output
#'
#' This might squeeze 4Gb of data for each folder...
#' @param path Path to the output of cellranger.
#' @param extreme Logical value if only some specific files should be kept.
#' @return Called by its side effect (It deletes some files and folders).
#' Returns `TRUE` if removed files and directories, `NULL` if cancelled, `FALSE`
#' if nothing to remove.
#' @importFrom utils askYesNo
#' @export
llrs_cellranger_clean <- function(path, extreme = FALSE) {
  path <- path[dir.exists(path)]
  light <- light_cleanup(path)
  extreme_res <- FALSE
  if (isTRUE(extreme)) {
    extreme_res <- extreme_cleanup(path)
  }
  light || extreme_res
}

light_cleanup <- function(path) {
  answer <- TRUE
  #  SC_MULTI_CS
  SC_MULTI_CS <- file.path(path, "SC_MULTI_CS")
  path_sample <- list.dirs(file.path(path, "outs", "per_sample_outs"),
                           recursive = FALSE)
  # All the analysis by default
  analysis <- file.path(path_sample, "count", "analysis")
  # All the internal files
  # Deleting this will make the folder not recognizable by this own function.
  files_ <- list.files(path, pattern = "^_", full.names = TRUE)
  # VDJ reference is duplicated on each sample!
  vdj_reference <- file.path(path, "outs", "vdj_reference")

  all_paths <- c(file.path(path, "SC_MULTI_CS"), files_, vdj_reference, analysis)
  all_paths <- all_paths[file.exists(all_paths) | dir.exists(all_paths)]

  if (length(all_paths) < 1) {
    return(FALSE)
  }

  if (check_cellranger_folder(path) && !check_cellranger_folder_version(path)) {
    message("This folder was not created with a version that has been tested")
    answer <- askYesNo("Are you sure you want to continue?", default = FALSE)
  } else {
    answer <- TRUE
  }

  if (isFALSE(answer) || is.na(answer)) {
    message("Cancelling")
    return(NULL)
  }

  message("Removing:", paste("\n -", all_paths))
  Sys.sleep(length(all_paths)/2)

  unlink(all_paths, recursive = TRUE)
  message("Removed files")
  TRUE
}

extreme_cleanup <- function(path) {

  files <- c(
    # Needed for easy aggr GEX before filtering
    "sample_molecule_info.h5",
    # Needed for easy parsing GEX and data analysis individually
    "sample_filtered_feature_bc_matrix.h5",
    # Needed for QC
    "metrics_summary.csv",
    "web_summary.html",
    # Needed for GEX in Loupe (after aggr)
    "cloupe.cloupe",
    # Needed for direct check of TCR (only relevant after aggr)
    "filtered_contig_annotations.csv",
    # Needed for aggr TCR info
    "vdj_contig_info.pb",
    # Needed for TCR in Loupe (multi or after aggr)
    "vloupe.vloupe"
  )

  all_f <- list.files(path = path, recursive = TRUE, full.names = TRUE)
  keep <- all_f[basename(all_f) %in% files]
  remove_f <- setdiff(all_f, keep)

  dirs <- dirname(all_f)
  dir_remove <- dirname(remove_f)
  remove_dir <- vector("character")
  for (dir in dirs) {
    files_in_dir <- all_f[dirs == dir]
    files_remove_in_dir <- remove_f[dir_remove == dir]
    if (all(files_in_dir %in% files_remove_in_dir)) {
      remove_dir <- c(remove_dir, dir)
    }
  }
  message("Removing:", paste("\n -", remove_f))
  Sys.sleep(length(remove_f)/2)

  if (length(remove_f)) {
    unlink(remove_f, recursive = TRUE)
    # Only those folders and not anything else
    unlink(remove_dir, recursive = FALSE)
    return(TRUE)
  } else {
    return(FALSE)
  }
}

check_cellranger_folder <- function(path) {
  any(file.exists(file.path(path, "_versions")))
}

check_cellranger_folder_version <- function(path, version = "7.2.0") {
  if (!check_installed("jsonlite")) {
    stop("Requires dependencies")
  }
  out <- vapply(file.path(path, "_versions"), jsonlite::read_json,
                FUN.VALUE = vector("list", 2))
  endsWith(simplify2array(out["pipelines", ]), version)
}

check_cellranger_version <- function(version = "7.2.0") {
  out <- system2("cellranger", "--version", stdout = TRUE)
  endsWith(out, version)
}

#' Aggregate several cellranger output
#'
#' If several samples cannot be processed with `cellranger multi` you might need to aggregate them.
#'
#' @param gex A data.frame with the GEX information.
#' @param path A path to were it should be saved.
#' @param vdj A data.frame with the VDJ information.
#' @return The path to the file saved with the configuration
#' @export
#' @importFrom utils write.csv
llrs_cellranger_aggr <- function(gex, path, vdj = NULL) {
  gex <- c("sample_id", "molecule_h5")
  tcr <- c("sample_id", "vdj_contig_info", "donor", "origin")
  stopifnot(colnames(gex) == gex)
  if (!all(basename(gex$molecule_h5) == "sample_molecule_info.h5")) {
    stop("Not all samples are sample_molecule_info.h5!", call. = FALSE)
  }

  if (is.null(vdj)) {
    write.csv(gex, file = path, quote = FALSE, row.names = FALSE)
  }
  stopifnot(colnames(vdj) == tcr)
  if (!all(basename(vdj$vdj_contig_info) == "vdj_contig_info.pb")) {
    stop("Not all samples are sample_molecule_info.h5!", call. = FALSE)

  }
  if (!is.null(vdj)) {
    write.csv(gex, file = path, quote = FALSE, row.names = FALSE)
  }

}

check_cellranger_vdj <- function(vdj) {
  vdj_columns <- c("reference", "inner-enrichment-primers", "r1-length", "r2-length")
  voc <- obligatory_columns(vdj, vdj_columns, 1)
  if (!voc) {
    stop("library does not have the required columns.", call. = FALSE)
  }
}

check_cellranger_gex <- function(gex) {
  gex_names <- c("reference", "probe-set", "filter-probes", "r1-length", "r2-length",
                 "chemistry",	"expect-cells",	"force-cells",	"no-secondary",
                 "no-bam",	"check-library-compatibility",	"include-introns",
                 "min-assignment-confidence")

  if (!is.list(gex) && !all(gex %in% gex_names)) {
    stop("The gene expression doesn't have all the fields.", call. = FALSE)
  }

  check_integer <- all(vapply(gex[gex_names[4:5, 7:8]], is.integer,
                              FUN.VALUE = logical(1L)))
  check_logical <- all(vapply(gex[gex_names[10:13]], is.logical,
                       FUN.VALUE = logical(1L)))
  check_numeric <- is.numeric(gex["min-assignment-confidence"])

  if (!any(check_integer, check_logical, check_numeric)) {
    stop("Some options are not correctly formatted.", call. = FALSE)
  }
  TRUE
}

check_cellranger_samples <- function(samples, library) {
  samples_columns <- c("sample_id", "expect_cells", "force_cells", "description", "cmo_ids", "probe_barcode_ids")
  soc <- obligatory_columns(samples, samples_columns, if ("3' Cell Multiplexing"){
    c(1, 5)} else if ("Fixed RNA Profiling"){6}else{ 1})
  if (!soc) {
    stop("Samples does not have the required columns.", call. = FALSE)
  }
  if (any(!samples$sample_id %in% library$fastq_id)) {
    stop("Samples id do not match with fastq ids")
  }
  # alphanumeric (I assume ASCII) with hyphens and/or underscores and less than 64 characters
  pattern <- "[^ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz[:digit:]_-]"
  # Pattern tested with c("cançó", "vinagre$", "mira-", "test_a", "test1")
  if (any(nchar(library$fastq_id) > 64) || any(grepl(pattern, library$fastq_id))) {
    stop("fastq_ids should be less than 64 characters")
  }
  TRUE
}

cat_csv <- function(df, out) {
  a <- apply(df, 1, paste, sep = ",", collapse = ",")
  cat(a, file = out, sep = "\n", append = TRUE)
}

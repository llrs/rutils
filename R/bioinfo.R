
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
  if (isTRUE(extreme)) {
    extreme_cleanup(path)
  } else {
    light_cleanup(path)
  }
}

light_cleanup <- function(path) {
  answer <- TRUE
  #  SC_MULTI_CS
  SC_MULTI_CS <- file.path(path, "SC_MULTI_CS")
  # All the internal files
  # Deleting this will make the folder not recognizable by this own function.
  files_ <- list.files(path, pattern = "^_", full.names = TRUE)
  # VDJ reference is duplicated on each sample!
  vdj_reference <- file.path(path, "outs", "vdj_reference")

  all_paths <- c(file.path(path, "SC_MULTI_CS"), files_, vdj_reference)
  all_paths <- all_paths[file.exists(all_paths) | dir.exists(all_paths)]

  if (length(all_paths) < 1) {
    return(FALSE)
  }

  if (any(!check_cellranger_folder(path))) {
    message("This folder was not created with a version that has been tested")
    answer <- askYesNo("Are you sure you want to continue?", default = FALSE)
  }

  if (isFALSE(answer) | is.na(answer)) {
    message("Cancelling")
    return(NULL)
  }

  message("Removing:", paste("\n -", all_paths))
  Sys.sleep(length(all_paths))

  unlink(SC_MULTI_CS[dir.exists(SC_MULTI_CS)], recursive = TRUE)
  unlink(files_[file.exists(files_)])
  unlink(vdj_reference[dir.exists(vdj_reference)], recursive = TRUE)
  message("Removed files")
  TRUE
}

extreme_cleanup <- function(path) {
  files <- paste(c("sample_molecule_info.h5",
                   "sample_filtered_feature_bc_matrix.h5",
                   "vdj_contig_info.pb",
                   "metrics_summary.csv",
                   "sample_cloupe.cloupe",
                   "vloupe.vloupe"),
                 collapse = "|")
  keep <- list.files(path = path, recursive = TRUE,
                     pattern = files, full.names = TRUE)
  all_f <- list.files(path = path, recursive = TRUE, full.names = TRUE)
  remove_f <- setdiff(all_f, keep)
  if (length(remove_f)) {
    unlink(remove_f, recursive = TRUE)
    # TODO: Remove empty folders...
    return(TRUE)
  } else {
    return(FALSE)
  }
}

check_cellranger_folder <- function(path, version = "7.2.0") {
  versions <- file.path(path, "_versions")
  if (!any(file.exists(versions))) {
    warning("Doesn't seem a cellranger output.")
    return(FALSE)
  }

  if (!check_installed("jsonlite")) {
    stop("Requires dependencies")
  }
  out <- vapply(versions, jsonlite::read_json, FUN.VALUE = vector("list", 2))
  endsWith(simplify2array(out["pipelines", ]), version)
}

check_cellranger_version <- function(version = "7.2.0") {
  out <- system2("cellranger", "--version", stdout = TRUE)
  endsWith(out, version)
}

#' Create multi config file
#'
#' Prepare the cellranger multi file for some samples.
#'
#' @param config A list with the general config of the samples.
#' @param gex A data.frame with the information for the gene expression (GEX).
#' @param library A data.frame with the information about the libraries used.
#' @param samples A data.frame with the information about the samples used.
#' @param vdj A list with the configuration for the vdj assays.
#' @param feature A list with the configuration for the featured assays.
#' @param out Path to a directory where to save the file.
#'
#' @return The path to a file saved with the configuration as specified.
#' @export
#' @seealso Set up names of samples with [llrs_cnag_files()]. Prepare folders and files [llrs_cnag_symlinks()].
#' @references <https://www.10xgenomics.com/support/software/cell-ranger/latest/advanced/cr-multi-config-csv-opts>
#' @examples
#' #llrs_cellranger_multi(out = "/S2_cellranger.csv")
llrs_cellranger_multi <- function(config, gex,
                                  library, samples, vdj, feature, out) {
  if (!dir.exists(dirname(out))) {
    stop("Directory doesn't exists and won't be created!")
  }
  if (file.exists(out)) {
    stop("Won't overwrite the existing file. Rename the output.")
  }

  if (length(dim(library)) != 2L) {
    stop("libraries should be a matrix or data.frame")
  }

  # GEX ####
  # [gene-expression]
  check_cellranger_gex(gex)


  # "reference",	/home/data/genome/human/refdata-gex-GRCh38-2020-A/
  # "probe-set",	/path/to/probe/set
  # "filter-probes",	<true|false>
  # "r1-length",	<int>
  # "r2-length",	<int>
  # "chemistry",	<auto>
  # "expect-cells",	<int>
  # "force-cells",	<int>
  # "no-secondary",	<true|false>
  # "no-bam",	<true|false>
  # "check-library-compatibility",	<true|false>
  # "include-introns",	<true|false>
  # "min-assignment-confidence",	<0.9>

  # vdj ####
  # [vdj] # For TCR and BCR libraries only
  check_cellranger_vdj(vdj)

  # Feature ####
  # [feature]
  feature_columns <- c("reference", "r1-length", "r2-length", )
  foc <- obligatory_columns(library, feature_columns, 1)
  if (!foc) {
    stop("library does not have the required columns.")
  }
  feature_options <- c("Gene Expression", "Antibody Capture",
                       "CRISPR Guide Capture", "Multiplexing Capture", "VDJ",
                       "VDJ-T", "VDJ-T-GD", "VDJ-B", "Antigen Capture", "Custom")



  # library ####
  library_columns <- c("fastq_id","fastqs","feature_types", "lanes",
                       "physical_library_id", "subsample_rate", "chemistry")
  loc <- obligatory_columns(library, library_columns, 1:3)
  if (!loc) {
    stop("library does not have the required columns.", call. = FALSE)
  }
  if (any(!is_dir(library$fastqs))) {
    stop("fastqs should be folders.", call. = FALSE)
  }

  arg <- match.arg(library$feature_type, feature_options)
  if (!all(startsWith(library$fastqs, "/"))) {
    stop("File should start with the absolute path.", call. = FALSE)
  }
  # antigen ####
  antigen_columns <- c("control_id", "mhc_allele")
  antigen <- NULL
  aoc <- obligatory_columns(antigen, antigen_columns)
  if (!aoc) {
    stop("Antigen specific is not well formated.")
  }

  # samples ####
  # It is needed if there are more than one sample to differentiate between them!
  if (NROW(library) >= 2 && (is.null(samples) || missing(samples))) {
    stop("There should be a samples section to differentiate the multiple samples")
  }



  cat(file = out, append = )
}

#' Aggregate several cellranger output
#'
#' If several samples cannot be processed with [llrs_cellranger_multi()] you might need to aggregate them.
#'
#' @param gex A data.frame with the GEX information.
#' @param path A path to were it should be saved.
#' @param vdj A data.frame with the VDJ information.
#' @return The path to the file saved with the configuration
#' @export
#' @importFrom utils write.csv
#' @seealso [llrs_cellranger_multi()]
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


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



#' Create multi file
#'
#' Prepare the cellranger multi file for some samples.
#' @param path Files to be used./ Info to be used.
#' @param out Path to a directory where to save the file.
#' @return The path to a file saved with the configuration as specified.
#' @export
#' @references <https://www.10xgenomics.com/support/software/cell-ranger/latest/advanced/cr-multi-config-csv-opts>
#' @examples
#' llrs_cellranger_multi(out = "/S2_cellranger.csv")
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

  # Feature ####
  # [vdj] # For TCR and BCR libraries only
  check_cellranger_vdj(vdj)
  feature_columns <- c("reference", "r1-length", "r2-length", )
  foc <- obligatory_columns(library, feature_columns, 1)
  if (!foc) {
    stop("library does not have the required columns.")
  }
  feature_options <- c("Gene Expression", "Antibody Capture",
                       "CRISPR Guide Capture", "Multiplexing Capture", "VDJ",
                       "VDJ-T", "VDJ-T-GD", "VDJ-B", "Antigen Capture", "Custom")

  # vdj ####
  # [vdj] # For TCR and BCR libraries only
  vdj_columns <- c("reference", "inner-enrichment-primers", "r1-length", "r2-length")
  voc <- obligatory_columns(library, vdj_columns, 1)
  if (!voc) {
    stop("library does not have the required columns.", call. = FALSE)
  }

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
  aoc <- obligatory_columns(antigen, antigen_columns)
  if (!aoc) {
    stop("Antigen specific is not well formated.")
  }

  # samples ####
  samples_columns <- c("sample_id", "expect_cells", "force_cells", "description", "cmo_ids", "probe_barcode_ids")
  soc <- obligatory_columns(samples, samples_columns, if ("3' Cell Multiplexing"){c(1, 5)} else if ("Fixed RNA Profiling"){6}else{ 1})
  if (!soc) {
    stop("Samples does not have the required columns.")
  }


  cat(file = out, append = )
}

#' Aggregate several cellrangers
#'
#' @return The path to the file saved with the configuration
#' @export
llrs_cellranger_aggr <- function() {
  tcr <- "sample_id,vdj_contig_info,donor,origin"
  gex <- "sample_id,molecule_h5"
  if (!endsWith(vdj_contig_info, ".pb")) {

  }
  if (!endsWith(molecule_h5, ".h5")) {
    stop
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

cat_csv <- function(df, out) {
  a <- apply(df, 1, paste, sep = ",", collapse = ",")
  cat(a, file = out, sep = "\n", append = TRUE)
}

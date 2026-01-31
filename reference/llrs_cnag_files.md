# Prepare CNAG data for cellranger

CNAG returns the data in a format that is not compatible with
cellranger. This function merges the data of CNAG to be able to prepare
for it.

## Usage

``` r
llrs_cnag_files(path)
```

## Arguments

- path:

  Path to a file with information about the delivery file.

## Value

A data.frame with the output of the names of the files ("d1" and "d2")
and its associated information.

## Details

The CNAG project is assumed to be below the file in `path` with all the
fastq files.

## Examples

``` r
# out <- llrs_cnag_cellranger("PROJECT_01.xls")
# out[, c("SAMPLE NAME", "d1", "cr1", "d1", "cr2")]
```

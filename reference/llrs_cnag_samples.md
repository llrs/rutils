# Create a sample sheet from files

Create a sample sheet from fastq.gz files for easier creation of sample
sheets for several programs (multiqc, fastqc, ...).

## Usage

``` r
llrs_cnag_samples(path)
```

## Arguments

- path:

  A path to a folder containing the files.

## Value

A data.frame with the names of the files.

## Details

Assumes a paired end sequencing.

## References

[CNAG](https://www.cnag.eu/)

## Examples

``` r
# llrs_cnag_samples("AUSER_01")
```

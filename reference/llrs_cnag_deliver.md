# Read the delivery file

Import the delivery file which has information about the machine,
application and samples provided by the researcher.

## Usage

``` r
llrs_cnag_deliver(path)
```

## Arguments

- path:

  Path to the project file.

## Value

A data.frame with the information in the project and its name and the
name of the fastq files of the project.

## References

[CNAG](https://www.cnag.eu/)

## Examples

``` r
# llrs_cnag_deliver("AUSER_01.xls")
```

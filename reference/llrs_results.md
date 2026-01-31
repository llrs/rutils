# Creates path for results

It creates a standardized name for results and checks that the
standardized folders exist.

## Usage

``` r
llrs_results(project, ...)
```

## Arguments

- project:

  Name of the project to be used in the title

- ...:

  Other names/variables. The extensions of the file should be included
  on the last element.

## Value

A path to a file.

## Examples

``` r
# llrs_results("rutils", "DEG", "limma", "a vs b.xlsx")
# llrs_results("rutils", "DEG", "pathways.xlsx")
```

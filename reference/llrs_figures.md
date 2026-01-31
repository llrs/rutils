# Creates path for a figure

creates a standardized name for figures and checks if the standardized
folders exist.

## Usage

``` r
llrs_figures(project, ...)
```

## Arguments

- project:

  Name of the project to be used in the title.

- ...:

  Other names/variables. The extensions of the file should be included
  on the last element.

## Value

A path to a file in a folder

## Examples

``` r
# llrs_figures("rutils", "pca", "sample1.png")
# llrs_figures("rutils", "pca", "sample whatever.png")
```

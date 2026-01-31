# Functions helping with targets

Prepare a target and serve it to the user

## Usage

``` r
llrs_tar_load(name, ...)

llrs_tar_read(name, ...)
```

## Arguments

- name:

  Name of the targets to prepare and load or read.

- ...:

  Other arguments passed around

## Value

Depending of what
[`targets::tar_load()`](https://docs.ropensci.org/targets/reference/tar_load.html)
and
[`targets::tar_read()`](https://docs.ropensci.org/targets/reference/tar_read.html)
returns.

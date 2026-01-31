# Clean cellranger multi output

This might squeeze 4Gb of data for each folder...

## Usage

``` r
llrs_cellranger_clean(path, extreme = FALSE)
```

## Arguments

- path:

  Path to the output of cellranger.

- extreme:

  Logical value if only some specific files should be kept.

## Value

Called by its side effect (It deletes some files and folders). Returns
`TRUE` if removed files and directories, `NULL` if cancelled, `FALSE` if
nothing to remove.

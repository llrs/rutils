# Create symlinks

Create symlinks for cellranger format (see references).

## Usage

``` r
llrs_cnag_symlinks(x, name, out_dir)
```

## Arguments

- x:

  A data.frame with the information. At least
  `c("LANE", "MULTIPLEX INDEX", "FLOWCELL", "d1", "d2")` columns must be
  present.

- name:

  The name of the sample, it can be a single character with the name of
  the column or a vector of characters with the sample name for each
  file.

- out_dir:

  Path to a folder where the files should be placed. If `NULL` it
  doesn't create the symlinks to the folder.

## Value

Called by its side effect but also returns a data.frame.

## References

<https://www.10xgenomics.com/support/software/cell-ranger/latest/analysis/inputs/cr-specifying-fastqs#file-naming-convention>

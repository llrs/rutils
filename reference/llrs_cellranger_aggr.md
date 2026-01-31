# Aggregate several cellranger output

If several samples cannot be processed with `cellranger multi` you might
need to aggregate them.

## Usage

``` r
llrs_cellranger_aggr(gex, path, vdj = NULL)
```

## Arguments

- gex:

  A data.frame with the GEX information.

- path:

  A path to were it should be saved.

- vdj:

  A data.frame with the VDJ information.

## Value

The path to the file saved with the configuration

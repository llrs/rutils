# Add column with

Adds a new column with those rows that meet the preference

## Usage

``` r
llrs_diff(
  x,
  p = "pval",
  fc = "diff",
  fdr = "fdr",
  fdr_threshold = 0.05,
  p_threshold = 0.05
)
```

## Arguments

- x:

  A data.frame or matrix with differential results (DEG or other
  bioinformatics data)

- p:

  The name of the column with the p-values.

- fc:

  The name of the column with the fold change.

- fdr:

  The name of the column with the p-values corrected by multiple
  comparison (usually FDR).

- fdr_threshold:

  The value to decide when to classify them.

- p_threshold:

  The value to decide when to consider a row

## Value

The same matrix or data.frame with a new column "sign" with 5 possible
values: `c("UUP", "UP", "", "DW", "DDW")`. This new column allows to
filter and easily find relevant rows.

## Note

limma recommends using the B value instead of this *ad-hoc* test (but it
is popular among labs).

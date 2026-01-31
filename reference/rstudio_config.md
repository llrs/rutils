# Switch between Rstudio configurations.

Switch between Rstudio configurations.

## Usage

``` r
llrs_rstudio_set()

llrs_rstudio_default()

llrs_rstudio_restore()
```

## Value

Called by its side effects.

## Functions

- `llrs_rstudio_set()`: Will setup the configuration.

- `llrs_rstudio_default()`: Will rename your configuration and restart
  RStudio, to revert to a fresh Rstudio with the default configuration
  (useful for teaching) or double checking default options.

- `llrs_rstudio_restore()`: Will restore your configuration.

## Examples

``` r
if (FALSE) { # \dontrun{
llrs_rstudio_set()
llrs_rstudio_default()
llrs_rstudio_restore()
} # }
```

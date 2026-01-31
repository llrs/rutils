# Copy config to config repository

Copy file to config repository and add it and push it.

## Usage

``` r
llrs_copy_config(path, dest_dir = "~/Documents/Projects/customizations")
```

## Arguments

- path:

  Path of file to copy

- dest_dir:

  Path to folder were to save it.

## Value

TRUE if successful

## Details

If there are files with the new file and the existing one it will prompt
to review them.

## Examples

``` r
if (FALSE) { # \dontrun{
llrs_copy_config("~/.gitconfig")
} # }
```

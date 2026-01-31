# List existing hooks

Check which hooks are in the package.

## Usage

``` r
llrs_hooks_list(full.names = FALSE)
```

## Arguments

- full.names:

  A logical value to be passed to list.files

## Value

A character vector with the name of the hooks.

## See also

[`llrs_hook()`](https://llrs.github.io/rutils/reference/llrs_hook.md)

## Examples

``` r
llrs_hooks_list()
#> [1] "post-receive" "pre-commit"   "pre-push"    
```

# Funding

Set github file for funding.

## Usage

``` r
llrs_funding(path = ".", ...)
```

## Arguments

- path:

  Path to project/package folder.

- ...:

  Other named strings that are [accepted by
  GitHub](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/customizing-your-repository/displaying-a-sponsor-button-in-your-repository)

## Value

Called by its side effects of creating a file.

## Examples

``` r
llrs_funding(path = tempdir())
```

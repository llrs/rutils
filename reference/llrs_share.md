# Create a link to a shared file

Uses the credentials to share a link to a resource.

## Usage

``` r
llrs_share(path, type, expiry, password, scope)
```

## Arguments

- path:

  The path to the file

- type:

  One of c("view", "edit", "embed").

- expiry:

  Time it is valid: "x days", "x hours".

- password:

  Password protected ?

- scope:

  I don't know.

## Value

A link to share a resource

## See also

[`llrs_download()`](https://llrs.github.io/rutils/reference/llrs_download.md)

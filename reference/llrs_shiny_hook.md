# Creates new shiny app and add hooks.

Creates new shiny app and add hooks.

## Usage

``` r
llrs_shiny_hook(project, path = "~/ShinyApps/", dest = "/srv/shiny-server/")
```

## Arguments

- project:

  Name of the project. It can be also multiple folders.

- path:

  Path to the folder where it should be.

- dest:

  Path to the shiny folder used to serve the website.

## Value

A logic value if everything went smoothly.

## See also

Wrapper on
[`llrs_shiny_create()`](https://llrs.github.io/rutils/reference/llrs_shiny_create.md)
and
[`llrs_hook()`](https://llrs.github.io/rutils/reference/llrs_hook.md).

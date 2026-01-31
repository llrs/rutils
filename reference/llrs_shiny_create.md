# Create shiny app

Create a shiny app in the right place as well as set the local git
repository to synchronize.

## Usage

``` r
llrs_shiny_create(project, path = "~/ShinyApps/", dest = "/srv/shiny-server/")
```

## Arguments

- project:

  Name of the project. It can be also multiple folders.

- path:

  Path to the folder where it should be.

- dest:

  Path to the shiny folder used to serve the website.

## Value

Called by its side effects. It will open a new RStudio project.

## See also

[`llrs_hook()`](https://llrs.github.io/rutils/reference/llrs_hook.md)

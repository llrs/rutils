# Insert hook

Insert a new hook (It doesn't replace hooks or expand them).

## Usage

``` r
llrs_hook(file, path = getwd())
```

## Arguments

- file:

  Name of the file to be used as a hook.

- path:

  Path to the git repository.

## Value

A logical value if the hook can be placed there.

## References

<https://git-scm.com/docs/githooks>

## See also

[`llrs_hooks_list()`](https://llrs.github.io/rutils/reference/llrs_hooks_list.md)

## Examples

``` r
if (FALSE) { # \dontrun{
hooks <- llrs_hooks_list()
llrs_hook(hooks[1], "projects/git_something")
} # }
```

# Add a contributor to the registry

Add a contributor to the registry

## Usage

``` r
llrs_add_member(
  name = getOption("usethis.full_name"),
  role = "Collaborator",
  path = "."
)
```

## Arguments

- name:

  Character name of the contributor.

- role:

  Either Collaborator or Publisher.

- path:

  Path to the project.

## Value

`NULL`

## See also

Other governance functions:
[`llrs_change_role()`](https://llrs.github.io/rutils/reference/llrs_change_role.md),
[`llrs_governance()`](https://llrs.github.io/rutils/reference/llrs_governance.md)

## Examples

``` r
if (FALSE) { # \dontrun{
llrs_governance()
llrs_add_member(NULL)
} # }
```

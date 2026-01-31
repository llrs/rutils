# Remove role of a contributor

Change a contributor role on the project.

## Usage

``` r
llrs_change_role(name = NULL, role = "User", path = ".")
```

## Arguments

- name:

  Character name of the contributor.

- role:

  Either User, Collaborator or Publisher.

- path:

  Path to the project.

## Value

`NULL`

## Examples

``` r
if (FALSE) { # \dontrun{
llrs_use_governance()
llrs_add_member(NULL)
llrs_change_member(NULL, "Publisher")
} # }
```

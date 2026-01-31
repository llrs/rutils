# File names of the CNAG projects.

Create the name of the files of the CNAG projects.

## Usage

``` r
llrs_cnag_project_file(project, n = 1L, path = "")

llrs_cnag_stats_file(project, n = 1L, path = "")

llrs_cnag_id(project, n = 1L)
```

## Arguments

- project:

  Name of the project.

- n:

  Integer value of the project (increasing from 1).

- path:

  Path to file.

## Value

A path for the stats or the file describing the samples of a project.

## Functions

- `llrs_cnag_project_file()`: Create a project file path.

- `llrs_cnag_stats_file()`: Create a project stats file path.

- `llrs_cnag_id()`: Create a project name from the id.

## Examples

``` r
llrs_cnag_id("project", 1:10)
#>  [1] "PROJECT_01" "PROJECT_02" "PROJECT_03" "PROJECT_04" "PROJECT_05"
#>  [6] "PROJECT_06" "PROJECT_07" "PROJECT_08" "PROJECT_09" "PROJECT_10"
llrs_cnag_project_file("project")
#> [1] "PROJECT_01/PROJECT_01.xls"
llrs_cnag_stats_file("project")
#> [1] "PROJECT_01/PROJECT_01_Sample_Stats.xls"
```

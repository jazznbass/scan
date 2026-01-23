# Add level-2 data to an scdf

Merges variables with corresponding case names from a data.frame with an
scdf.

## Usage

``` r
add_l2(scdf, data_l2, cvar = "case")
```

## Arguments

- scdf:

  A single-case data frame. See
  [`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md) to
  learn about this format.

- data_l2:

  A level 2 dataset.

- cvar:

  Character string with the name of the "case" variable in the L2
  dataset (default is 'case').

## Value

An scdf with added level-2 variables.

## Details

This function is mostly used in combination with the
[`hplm()`](https://jazznbass.github.io/scan/reference/hplm.md) function.
It adds level-2 variables to each single-case data frame in an scdf
based on matching case names.

## See also

[`hplm()`](https://jazznbass.github.io/scan/reference/hplm.md)

Other data manipulation functions:
[`as.data.frame.scdf()`](https://jazznbass.github.io/scan/reference/as.data.frame.scdf.md),
[`as_scdf()`](https://jazznbass.github.io/scan/reference/as_scdf.md),
[`batch_apply()`](https://jazznbass.github.io/scan/reference/batch_apply.md),
[`fill_missing()`](https://jazznbass.github.io/scan/reference/fill_missing.md),
[`moving_median()`](https://jazznbass.github.io/scan/reference/transform.scdf.md),
[`print.sc_outlier()`](https://jazznbass.github.io/scan/reference/outlier.md),
[`ranks()`](https://jazznbass.github.io/scan/reference/ranks.md),
[`rescale()`](https://jazznbass.github.io/scan/reference/rescale.md),
[`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md),
[`select_cases()`](https://jazznbass.github.io/scan/reference/select_cases.md),
[`set_vars()`](https://jazznbass.github.io/scan/reference/set_vars.md),
[`shift()`](https://jazznbass.github.io/scan/reference/shift.md),
[`smooth_cases()`](https://jazznbass.github.io/scan/reference/smooth_cases.md),
[`standardize()`](https://jazznbass.github.io/scan/reference/standardize.md),
[`truncate_phase()`](https://jazznbass.github.io/scan/reference/truncate_phase.md)

## Author

Juergen Wilbert

## Examples

``` r
## Example with the default case variable name 'case'
Leidig2018 |> add_l2(Leidig2018_l2)
#> #A single-case data frame with 35 cases
#> 
#>  1a1: academic_engagement mt classID weekday disruptive_behavior phase class
#>                         4  1      1a       3                   1     A    1a
#>                         1  2      1a       4                   1     A    1a
#>                         2  3      1a       5                   1     A    1a
#>                      <NA>  4      1a       1                <NA>     A    1a
#>                         2  5      1a       2                   1     A    1a
#>                         3  6      1a       3                   1     A    1a
#>                         1  7      1a       4                   1     A    1a
#>                         1  8      1a       5                   2     A    1a
#>                         3  9      1a       1                   0     B    1a
#>                         4 10      1a       2                   0     B    1a
#>                         3 11      1a       3                   0     B    1a
#>                         4 12      1a       4                   1     B    1a
#>                         4 13      1a       5                   0     B    1a
#>                         4 14      1a       1                   0     B    1a
#>                         4 15      1a       2                   0     B    1a
#>  gender migration first_language_german SDQ_TOTAL SDQ_EXTERNALIZING
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>  SDQ_INTERNALIZING ITRF_TOTAL ITRF_ACADEMIC ITRF_BEHAVIOR
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#> # ... up to 93 more rows
#> #  34 more cases
## Example with a different case variable name in the L2 data
Leidig2018_l2_renamed <- Leidig2018_l2
names(Leidig2018_l2_renamed)[2] <- "subject"
Leidig2018 |> add_l2(Leidig2018_l2_renamed, cvar = "subject")
#> #A single-case data frame with 35 cases
#> 
#>  1a1: academic_engagement mt classID weekday disruptive_behavior phase class
#>                         4  1      1a       3                   1     A    1a
#>                         1  2      1a       4                   1     A    1a
#>                         2  3      1a       5                   1     A    1a
#>                      <NA>  4      1a       1                <NA>     A    1a
#>                         2  5      1a       2                   1     A    1a
#>                         3  6      1a       3                   1     A    1a
#>                         1  7      1a       4                   1     A    1a
#>                         1  8      1a       5                   2     A    1a
#>                         3  9      1a       1                   0     B    1a
#>                         4 10      1a       2                   0     B    1a
#>                         3 11      1a       3                   0     B    1a
#>                         4 12      1a       4                   1     B    1a
#>                         4 13      1a       5                   0     B    1a
#>                         4 14      1a       1                   0     B    1a
#>                         4 15      1a       2                   0     B    1a
#>  gender migration first_language_german SDQ_TOTAL SDQ_EXTERNALIZING
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>       0         0                     1        10                 9
#>  SDQ_INTERNALIZING ITRF_TOTAL ITRF_ACADEMIC ITRF_BEHAVIOR
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#>                  1         11             7             4
#> # ... up to 93 more rows
#> #  34 more cases
```

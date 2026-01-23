# Select a subset of cases from an scdf

This function allows to select a subset of cases from an scdf by
specifying either the case names or their numeric indices. Negative
selection is also supported.

## Usage

``` r
select_cases(scdf, ...)
```

## Arguments

- scdf:

  A single-case data frame. See
  [`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md) to
  learn about this format.

- ...:

  Selection criteria. Either numeric, objectnames, or as characters.

## Value

An scdf with a subset of cases.

## See also

Other data manipulation functions:
[`add_l2()`](https://jazznbass.github.io/scan/reference/add_l2.md),
[`as.data.frame.scdf()`](https://jazznbass.github.io/scan/reference/as.data.frame.scdf.md),
[`as_scdf()`](https://jazznbass.github.io/scan/reference/as_scdf.md),
[`batch_apply()`](https://jazznbass.github.io/scan/reference/batch_apply.md),
[`fill_missing()`](https://jazznbass.github.io/scan/reference/fill_missing.md),
[`moving_median()`](https://jazznbass.github.io/scan/reference/transform.scdf.md),
[`print.sc_outlier()`](https://jazznbass.github.io/scan/reference/outlier.md),
[`ranks()`](https://jazznbass.github.io/scan/reference/ranks.md),
[`rescale()`](https://jazznbass.github.io/scan/reference/rescale.md),
[`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md),
[`set_vars()`](https://jazznbass.github.io/scan/reference/set_vars.md),
[`shift()`](https://jazznbass.github.io/scan/reference/shift.md),
[`smooth_cases()`](https://jazznbass.github.io/scan/reference/smooth_cases.md),
[`standardize()`](https://jazznbass.github.io/scan/reference/standardize.md),
[`truncate_phase()`](https://jazznbass.github.io/scan/reference/truncate_phase.md)

## Author

Juergen Wilbert

## Examples

``` r
select_cases(exampleAB, Johanna, Karolina)
#> #A single-case data frame with two cases
#> 
#>  Johanna: values mt phase │ Karolina: values mt phase │
#>               54  1     A │               41  1     A │
#>               53  2     A │               59  2     A │
#>               56  3     A │               56  3     A │
#>               58  4     A │               51  4     A │
#>               52  5     A │               52  5     A │
#>               61  6     B │               57  6     B │
#>               62  7     B │               56  7     B │
#>               71  8     B │               67  8     B │
#>               66  9     B │               75  9     B │
#>               64 10     B │               66 10     B │
#>               78 11     B │               69 11     B │
#>               70 12     B │               68 12     B │
#>               74 13     B │               73 13     B │
#>               82 14     B │               77 14     B │
#>               77 15     B │               79 15     B │
#> # ... up to five more rows
select_cases(exampleAB, c(Johanna, Karolina))
#> #A single-case data frame with two cases
#> 
#>  Johanna: values mt phase │ Karolina: values mt phase │
#>               54  1     A │               41  1     A │
#>               53  2     A │               59  2     A │
#>               56  3     A │               56  3     A │
#>               58  4     A │               51  4     A │
#>               52  5     A │               52  5     A │
#>               61  6     B │               57  6     B │
#>               62  7     B │               56  7     B │
#>               71  8     B │               67  8     B │
#>               66  9     B │               75  9     B │
#>               64 10     B │               66 10     B │
#>               78 11     B │               69 11     B │
#>               70 12     B │               68 12     B │
#>               74 13     B │               73 13     B │
#>               82 14     B │               77 14     B │
#>               77 15     B │               79 15     B │
#> # ... up to five more rows
select_cases(exampleAB, 1,2)
#> #A single-case data frame with two cases
#> 
#>  Johanna: values mt phase │ Karolina: values mt phase │
#>               54  1     A │               41  1     A │
#>               53  2     A │               59  2     A │
#>               56  3     A │               56  3     A │
#>               58  4     A │               51  4     A │
#>               52  5     A │               52  5     A │
#>               61  6     B │               57  6     B │
#>               62  7     B │               56  7     B │
#>               71  8     B │               67  8     B │
#>               66  9     B │               75  9     B │
#>               64 10     B │               66 10     B │
#>               78 11     B │               69 11     B │
#>               70 12     B │               68 12     B │
#>               74 13     B │               73 13     B │
#>               82 14     B │               77 14     B │
#>               77 15     B │               79 15     B │
#> # ... up to five more rows
select_cases(exampleAB, 1:2)
#> #A single-case data frame with two cases
#> 
#>  Johanna: values mt phase │ Karolina: values mt phase │
#>               54  1     A │               41  1     A │
#>               53  2     A │               59  2     A │
#>               56  3     A │               56  3     A │
#>               58  4     A │               51  4     A │
#>               52  5     A │               52  5     A │
#>               61  6     B │               57  6     B │
#>               62  7     B │               56  7     B │
#>               71  8     B │               67  8     B │
#>               66  9     B │               75  9     B │
#>               64 10     B │               66 10     B │
#>               78 11     B │               69 11     B │
#>               70 12     B │               68 12     B │
#>               74 13     B │               73 13     B │
#>               82 14     B │               77 14     B │
#>               77 15     B │               79 15     B │
#> # ... up to five more rows
select_cases(exampleAB, -Johanna)
#> #A single-case data frame with two cases
#> 
#>  Karolina: values mt phase │ Anja: values mt phase │
#>                41  1     A │           55  1     A │
#>                59  2     A │           58  2     A │
#>                56  3     A │           53  3     A │
#>                51  4     A │           50  4     A │
#>                52  5     A │           52  5     A │
#>                57  6     B │           55  6     B │
#>                56  7     B │           68  7     B │
#>                67  8     B │           68  8     B │
#>                75  9     B │           81  9     B │
#>                66 10     B │           67 10     B │
#>                69 11     B │           78 11     B │
#>                68 12     B │           73 12     B │
#>                73 13     B │           72 13     B │
#>                77 14     B │           78 14     B │
#>                79 15     B │           81 15     B │
#> # ... up to five more rows
select_cases(exampleAB, -c(Johanna, Karolina))
#> #A single-case data frame with one case
#> 
#>  Anja: values mt phase
#>            55  1     A
#>            58  2     A
#>            53  3     A
#>            50  4     A
#>            52  5     A
#>            55  6     B
#>            68  7     B
#>            68  8     B
#>            81  9     B
#>            67 10     B
#>            78 11     B
#>            73 12     B
#>            72 13     B
#>            78 14     B
#>            81 15     B
#> # ... up to five more rows
v <- c("Moritz", "Jannis")
select_cases(exampleA1B1A2B2, v)
#> Error in eval(x, envir = nl, enclos = parent.frame()): object 'v' not found
```

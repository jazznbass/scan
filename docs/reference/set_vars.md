# Set analysis variables in an scdf object

This function allows to set or change the dependent variable,
measurement-time variable, and phase variable in an scdf object.

## Usage

``` r
set_vars(data, dvar, mvar, pvar)

set_dvar(data, dvar)

set_mvar(data, mvar)

set_pvar(data, pvar)
```

## Arguments

- data:

  A single-case data frame. See
  [`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md) to
  learn about this format.

- dvar:

  Character string. Name of the dependent variable.

- mvar:

  Character string. Name of the measurement-time variable.

- pvar:

  Character string. Name of the phase variable.

## Value

An `scdf` object with updated variable settings.

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
[`select_cases()`](https://jazznbass.github.io/scan/reference/select_cases.md),
[`shift()`](https://jazznbass.github.io/scan/reference/shift.md),
[`smooth_cases()`](https://jazznbass.github.io/scan/reference/smooth_cases.md),
[`standardize()`](https://jazznbass.github.io/scan/reference/standardize.md),
[`truncate_phase()`](https://jazznbass.github.io/scan/reference/truncate_phase.md)

## Author

Juergen Wilbert

## Examples

``` r
exampleAB_add |>
  set_dvar("depression") |>
  describe()
#> Describe Single-Case Data
#> 
#>                           Rolf
#> Design         Base-Medication
#> n.Base                      15
#> n.Medication                25
#> mis.Base                     0
#> mis.Medication               0
#> 
#>                    Rolf
#> m.Base                5
#> m.Medication       4.36
#> md.Base               6
#> md.Medication         3
#> sd.Base           2.673
#> sd.Medication     3.252
#> mad.Base          2.965
#> mad.Medication    2.965
#> min.Base              0
#> min.Medication        0
#> max.Base              9
#> max.Medication       10
#> trend.Base        0.114
#> trend.Medication -0.051
#> 
#> The following variables were used in this analysis:
#> 'depression' as dependent variable, 'phase' as phase variable, and 'day' as measurement-time variable.
```

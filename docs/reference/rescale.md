# Rescales values of an scdf

This function scales the measured values of an scdf file. It allows for
mean centering and standardization across all cases included in an scdf.

## Usage

``` r
rescale(data, ..., m = 0, sd = 1)
```

## Arguments

- data:

  A single-case data frame. See
  [`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md) to
  learn about this format.

- ...:

  Names of variables to be standardized. If none are given, all numeric
  variables are standardized.

- m:

  The target mean. If set NULL, it is not changed.

- sd:

  The target standard deviation. If set NULL, it is not changed.

## Value

An scdf with the scaled values.

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
## Standardize a multiple case scdf and compute an hplm
exampleAB_50 |>
  rescale(values, mt) |>
  hplm()
#> Hierarchical Piecewise Linear Regression
#> 
#> Estimation method ML 
#> Contrast model: W / level: first, slope: first
#> 50 Cases
#> 
#> AIC = 508.2253, BIC = 539.6087
#> ICC = 0.287; L = 339.0; p = 0.000 
#> 
#> Fixed effects (values ~ 1 + mt + phaseB + interB)
#> 
#>                             B    SE   df       t p
#> Intercept              -1.251 0.075 1328 -16.716 0
#> Trend (mt)              0.242 0.048 1328   5.006 0
#> Level phase B (phaseB)  0.708 0.033 1328  21.436 0
#> Slope phase B (interB)  0.377 0.050 1328   7.588 0
#> 
#> Random effects (~1 | case)
#> 
#>              SD
#> Intercept 0.503
#> Residual  0.266
```

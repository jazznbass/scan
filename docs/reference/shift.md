# Shift values in a single-case data file

*This function has been superseded by the much more versatile
[`transform.scdf`](https://jazznbass.github.io/scan/reference/transform.scdf.md)
function.* Shifting the values might be helpful in cases where the
measurement time is given as a time variable (see example below).

## Usage

``` r
shift(data, value, var)
```

## Arguments

- data:

  A single-case data frame. See
  [`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md) to
  learn about this format.

- value:

  Number by which to shift the values

- var:

  Character string with the name of the target variable. Defaults to the
  measurement time variable.

## Value

A scdf with shifted data

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
[`set_vars()`](https://jazznbass.github.io/scan/reference/set_vars.md),
[`smooth_cases()`](https://jazznbass.github.io/scan/reference/smooth_cases.md),
[`standardize()`](https://jazznbass.github.io/scan/reference/standardize.md),
[`truncate_phase()`](https://jazznbass.github.io/scan/reference/truncate_phase.md)

## Examples

``` r
### Shift the measurement time for a better estimation of the intercept
ex <- shift(example_A24, value = -1996)
#> Warning: This function is deprecated. It might be dropped without any further notice in a future update of scan.
#> Please use function 'transform' instead of 'shift'.
plm(ex)
#> Piecewise Regression Analysis
#> 
#> Contrast model: W / level = first, slope = first
#> 
#> Fitted a gaussian distribution.
#> F(3, 19) = 46.74; p = 0.000; R² = 0.881; Adjusted R² = 0.862; AIC = 221.5733
#> 
#>                               B LL-CI95% UL-CI95%     SE      t     p delta R²
#> Intercept               258.714  223.364  294.065 18.036 14.344 0.000         
#> Trend (year)              1.857   -7.947   11.662  5.002  0.371 0.715    0.001
#> Level phase B (phaseB) -150.383 -200.743 -100.024 25.694 -5.853 0.000    0.215
#> Slope phase B (interB)   -1.726  -11.926    8.474  5.204 -0.332 0.744    0.001
#> 
#> Autocorrelations of the residuals
#>  lag    cr
#>    1  0.10
#>    2 -0.13
#>    3 -0.10
#> Ljung-Box test: X²(3) = 0.99; p = 0.804 
#> 
#> Formula: injuries ~ 1 + year + phaseB + interB 

# Please use transform instead:
example_A24 |>
  transform(year = year - 1996) |>
  plm()
#> Piecewise Regression Analysis
#> 
#> Contrast model: W / level = first, slope = first
#> 
#> Fitted a gaussian distribution.
#> F(3, 19) = 46.74; p = 0.000; R² = 0.881; Adjusted R² = 0.862; AIC = 221.5733
#> 
#>                               B LL-CI95% UL-CI95%     SE      t     p delta R²
#> Intercept               258.714  223.364  294.065 18.036 14.344 0.000         
#> Trend (year)              1.857   -7.947   11.662  5.002  0.371 0.715    0.001
#> Level phase B (phaseB) -150.383 -200.743 -100.024 25.694 -5.853 0.000    0.215
#> Slope phase B (interB)   -1.726  -11.926    8.474  5.204 -0.332 0.744    0.001
#> 
#> Autocorrelations of the residuals
#>  lag    cr
#>    1  0.10
#>    2 -0.13
#>    3 -0.10
#> Ljung-Box test: X²(3) = 0.99; p = 0.804 
#> 
#> Formula: injuries ~ 1 + year + phaseB + interB 
```

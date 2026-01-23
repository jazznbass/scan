# Smoothing single-case data

*This function is superseded by the more versatile
[`transform.scdf`](https://jazznbass.github.io/scan/reference/transform.scdf.md)
function.* The `smooth_cases` function provides procedures to smooth
single-case data (i.e., to eliminate noise). A moving average function
(mean- or median-based) replaces each data point by the average of the
surrounding data points step-by-step. With a local regression function,
each data point is regressed by its surrounding data points.

## Usage

``` r
smooth_cases(data, dvar, mvar, method = "mean", intensity = NULL, FUN = NULL)
```

## Arguments

- data:

  A single-case data frame. See
  [`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md) to
  learn about this format.

- dvar:

  Character string with the name of the dependent variable. Defaults to
  the attributes in the scdf file.

- mvar:

  Character string with the name of the measurement time variable.
  Defaults to the attributes in the scdf file.

- method, FUN:

  Method determining the smoothed scores. Default `method = "median"` is
  a moving median function. Further possible values are: `"mean"` and a
  non-parametric `"regression"`.

- intensity:

  For `method = "median"` and `"mean"` it is the lag used for computing
  the average. Default is `intensity = 1`. In case of
  `method = "regression"` it is the proportion of surrounding data
  influencing each data point, which is `intensity = 0.2` by default.

## Value

Returns a data frame (for each single-case) with smoothed data points.
See [`scdf`](https://jazznbass.github.io/scan/reference/scdf.md) to
learn about the format of these data frames.

## Details

`moving_median`, `moving_mean`, and `local_regression` are helper
function for
[`transform.scdf`](https://jazznbass.github.io/scan/reference/transform.scdf.md)
returning the smoothed values of a numeric vector.

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
[`shift()`](https://jazznbass.github.io/scan/reference/shift.md),
[`standardize()`](https://jazznbass.github.io/scan/reference/standardize.md),
[`truncate_phase()`](https://jazznbass.github.io/scan/reference/truncate_phase.md)

## Author

Juergen Wilbert

## Examples

``` r
## Use the three different smoothing functions and compare the results
study <- c(
  "Original" = Huber2014$Berta,
  "Moving median" = smooth_cases(Huber2014$Berta, method = "median"),
  "Moving mean" = smooth_cases(Huber2014$Berta, method = "mean"),
  "Local regression" = smooth_cases(Huber2014$Berta, method = "regression")
)
plot(study)
#> Warning: This function is deprecated. It might be dropped without any further notice in a future update of scan.
#> Please use function 'scplot' from the package 'scplot' instead of 'plot'.
#> Warning: This function is deprecated. It might be dropped without any further notice in a future update of scan.
#> Please use function 'scplot' from the package 'scplot' instead of 'style_plot'.


Huber2014$Berta |>
transform(
  "compliance (moving median)" = moving_median(compliance),
  "compliance (moving mean)" = moving_mean(compliance),
  "compliance (local regression)" = local_regression(compliance, mt)
)
#> #A single-case data frame with one case
#> 
#>  Berta: compliance mt phase compliance (moving median) compliance (moving mean)
#>                 25  1     A                         25                       25
#>               20.8  2     A                         25                    28.47
#>               39.6  3     A                       39.6                    47.69
#>                 75  4     A                         45                     55.9
#>                 45  5     A                         45                     38.5
#>               14.6  6     A                         45                    32.97
#>               45.8  7     A                         45                    37.36
#>               33.3  8     A                       33.3                    33.99
#>               31.3  9     A                       32.5                     32.6
#>               32.5 10     A                       32.5                     23.1
#>                4.2 11     B                       32.5                    24.37
#>               45.8 12     B                       32.5                    33.82
#>               31.3 13     B                       31.3                    24.84
#>                9.4 14     B                        9.4                    13.51
#>                6.3 15     B                        6.3                        8
#>  compliance (local regression)
#>                          22.02
#>                          28.81
#>                           39.6
#>                           42.3
#>                             45
#>                           45.4
#>                           45.8
#>                          36.07
#>                           32.2
#>                           32.5
#>                          39.15
#>                           45.8
#>                          29.41
#>                          15.05
#>                           6.58
#> # ... up to 14 more rows
```

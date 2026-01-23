# Apply a function to each element in an scdf.

This function applies a given function to each case of a multiple case
scdf, returning a list of the output of each function call.

## Usage

``` r
batch_apply(scdf, fn, simplify = FALSE)
```

## Arguments

- scdf:

  A list of inputs to apply the function to.

- fn:

  The function to apply to each element. Use a `.` as a placeholder for
  the scdf (e.g. `describe(.)`).

- simplify:

  If simplify is TRUE and `fn` returns a vector of values, `batch_apply`
  will return a data frame case names.

## Value

A list of the output of each function call.

## Details

If `simplify` is TRUE and the function returns a vector of values, the
output is combined into a data frame with case names as additional
columns.

This is particularly useful for applying statistical models or summary
statistics to each case in an scdf.

## See also

Other data manipulation functions:
[`add_l2()`](https://jazznbass.github.io/scan/reference/add_l2.md),
[`as.data.frame.scdf()`](https://jazznbass.github.io/scan/reference/as.data.frame.scdf.md),
[`as_scdf()`](https://jazznbass.github.io/scan/reference/as_scdf.md),
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
batch_apply(exampleAB, coef(plm(.)))
#> $Johanna
#>              Estimate Std. Error     t value     Pr(>|t|)
#> (Intercept) 54.400000   3.889690 13.98569074 2.173001e-10
#> mt           0.100000   1.587959  0.06297391 9.505673e-01
#> phaseB       7.858333   5.816499  1.35104178 1.954791e-01
#> interB       1.525000   1.616067  0.94364905 3.593813e-01
#> 
#> $Karolina
#>               Estimate Std. Error    t value     Pr(>|t|)
#> (Intercept) 49.0000000   4.098328 11.9560945 2.169640e-09
#> mt           1.4000000   1.673136  0.8367523 4.150550e-01
#> phaseB       3.8916667   6.128489  0.6350124 5.343967e-01
#> interB       0.5392857   1.702751  0.3167144 7.555551e-01
#> 
#> $Anja
#>             Estimate Std. Error    t value     Pr(>|t|)
#> (Intercept) 56.40000   4.250294 13.2696700 4.729808e-10
#> mt          -1.40000   1.735175 -0.8068349 4.315923e-01
#> phaseB      16.96667   6.355733  2.6695057 1.678721e-02
#> interB       2.50000   1.765889  1.4157177 1.760295e-01
#> 
```

# (Deprecated) Rank-transformation of single-case data files

*This function is superseded by the more versatile
[`transform.scdf`](https://jazznbass.github.io/scan/reference/transform.scdf.md)
function.*

## Usage

``` r
ranks(data, var, grand = TRUE, ...)
```

## Arguments

- data:

  A single-case data frame. See
  [`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md) to
  learn about this format.

- var:

  A string or string vector with the names of the variables to be
  ranked.

- grand:

  If TRUE, ranks will be calculated across all cases. If FALSE ranks are
  calculated within each case.

- ...:

  Additional parameters passed to the
  [`rank`](https://rdrr.io/r/base/rank.html) function.

## Value

An `scdf` object where the values of the variable(s) are replaced with
ranks.

## See also

Other data manipulation functions:
[`add_l2()`](https://jazznbass.github.io/scan/reference/add_l2.md),
[`as.data.frame.scdf()`](https://jazznbass.github.io/scan/reference/as.data.frame.scdf.md),
[`as_scdf()`](https://jazznbass.github.io/scan/reference/as_scdf.md),
[`batch_apply()`](https://jazznbass.github.io/scan/reference/batch_apply.md),
[`fill_missing()`](https://jazznbass.github.io/scan/reference/fill_missing.md),
[`moving_median()`](https://jazznbass.github.io/scan/reference/transform.scdf.md),
[`print.sc_outlier()`](https://jazznbass.github.io/scan/reference/outlier.md),
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
# The ranks function is deprecated. Please use transform:
res1 <- ranks(Huber2014, var = "compliance")
res2 <- transform(Huber2014, across_cases(compliance = rank(compliance, na.last="keep")))
identical(res1, res2)
#> [1] TRUE

res1 <- ranks(Huber2014, var = "compliance", grand = FALSE)
res2 <- transform(Huber2014, compliance = rank(compliance, na.last="keep"))
identical(res1, res2)
#> [1] TRUE
```

# Trend analysis for single-cases data

The `trend()` function provides an overview of linear trends in single
case data. By default, it provides the intercept and slope of a linear
and quadratic regression of measurement time on scores. Models are
calculated separately for each phase and across all phases. For more
advanced use, you can add regression models using the R-specific formula
class.

## Usage

``` r
trend(
  data,
  dvar,
  pvar,
  mvar,
  offset = "deprecated",
  first_mt = 0,
  model = NULL
)
```

## Arguments

- data:

  A single-case data frame. See
  [`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md) to
  learn about this format.

- dvar:

  Character string with the name of the dependent variable. Defaults to
  the attributes in the scdf file.

- pvar:

  Character string with the name of the phase variable. Defaults to the
  attributes in the scdf file.

- mvar:

  Character string with the name of the measurement time variable.
  Defaults to the attributes in the scdf file.

- offset:

  (Deprecated. Please use first_mt). An offset for the first
  measurement-time of each phase. If `offset = 0`, the phase measurement
  is handled as MT 1. Default is `offset = -1`, setting the first value
  of MT to 0.

- first_mt:

  A numeric setting the value for the first measurement-time. Default =
  0.

- model:

  A string or a list of (named) strings each depicting one regression
  model. This is a formula expression of the standard R class. The
  parameters of the model are `values`, `mt` and `phase`.

## Value

A list of class `sc_trend` containing:

- trend:

  A matrix containing the results (Intercept, B and beta) of separate
  regression models for phase A, phase B, and the whole data.

- first_mt:

  Numeric argument from function call (see arguments section).

## Details

The function computes separate regression models for each phase and for
the whole data. By default two models are computed: a linear model and a
quadratic model. Additionally, custom models can be specified using the
`model` argument. The measurement time variable is adjusted such that
the first measurement time point of each phase is set to the value
specified in the `first_mt` argument (default = 0). This means that if
`first_mt = 0`, the first measurement time point of each phase is set to
0, if `first_mt = 1`, the first measurement time point of each phase is
set to 1, and so on. This adjustment allows for a more intuitive
interpretation of the regression coefficients, especially the intercept,
which then represents the estimated value at the beginning of each
phase.

## See also

[`describe()`](https://jazznbass.github.io/scan/reference/describe.md)

Other regression functions:
[`bplm()`](https://jazznbass.github.io/scan/reference/bplm.md),
[`fetch()`](https://jazznbass.github.io/scan/reference/fetch.md),
[`hplm()`](https://jazznbass.github.io/scan/reference/hplm.md),
[`mplm()`](https://jazznbass.github.io/scan/reference/mplm.md),
[`plm()`](https://jazznbass.github.io/scan/reference/plm.md),
[`print.sc_ac()`](https://jazznbass.github.io/scan/reference/autocorr.md),
[`print.sc_bctau()`](https://jazznbass.github.io/scan/reference/corrected_tau.md)

## Author

Juergen Wilbert

## Examples

``` r
## Compute the linear and squared regression for a random single-case
design <- design(slope = 0.5)
matthea <- random_scdf(design)
trend(matthea)
#> Trend for each phase
#> 
#>               Intercept     B  Beta
#> Linear.ALL       41.504 4.159 0.957
#> Linear.A         50.958 0.750 0.320
#> Linear.B         57.269 4.751 0.946
#> Quadratic.ALL    54.904 0.211 0.958
#> Quadratic.A      52.085 0.062 0.110
#> Quadratic.B      68.993 0.318 0.920
#> 
#> Note. Measurement-times start at 0 for each phase

## Besides the linear and squared regression models compute two custom models:
## a) a cubic model, and
## b) the values predicted by the natural logarithm of the
## measurement time.
design <- design(slope = 0.3)
ben <- random_scdf(design)
trend(
  ben,
  model = list("Cubic" = values ~ mt^3, "Log Time" = values ~ log(mt)),
  first_mt = 1 # must be set to 1 because log(0) would be -Inf
)
#> Trend for each phase
#> 
#>               Intercept      B   Beta
#> Linear.ALL       43.260  2.417  0.898
#> Linear.A         56.353 -0.794 -0.272
#> Linear.B         48.333  3.149  0.918
#> Quadratic.ALL    52.101  0.115  0.926
#> Quadratic.A      56.289 -0.211 -0.441
#> Quadratic.B      58.126  0.186  0.893
#> Cubic.ALL        43.260  2.417  0.898
#> Cubic.A          56.353 -0.794 -0.272
#> Cubic.B          48.333  3.149  0.918
#> Log Time.ALL     37.394 14.760  0.753
#> Log Time.A       54.527 -0.581 -0.080
#> Log Time.B       42.725 16.561  0.844
#> 
#> Note. Measurement-times start at 1 for each phase
```

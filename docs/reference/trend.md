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

# S3 method for class 'sc_trend'
print(x, digits = 3, ...)

# S3 method for class 'sc_trend'
export(
  object,
  caption = NA,
  footnote = NA,
  filename = NA,
  round = 3,
  decimals = NULL,
  ...
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
  parameters of the model are `values` and `mt`.

- ...:

  Further arguments passed to the function.

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

## Functions

- `print(sc_trend)`: Print results

- `export(sc_trend)`: Export results as html table (see
  [`export()`](https://jazznbass.github.io/scan/reference/export.md))

## See also

[`describe()`](https://jazznbass.github.io/scan/reference/describe.md)

Other regression functions:
[`autocorr()`](https://jazznbass.github.io/scan/reference/autocorr.md),
[`bplm()`](https://jazznbass.github.io/scan/reference/bplm.md),
[`corrected_tau()`](https://jazznbass.github.io/scan/reference/corrected_tau.md),
[`fetch()`](https://jazznbass.github.io/scan/reference/fetch.md),
[`hplm()`](https://jazznbass.github.io/scan/reference/hplm.md),
[`mplm()`](https://jazznbass.github.io/scan/reference/mplm.md),
[`plm()`](https://jazznbass.github.io/scan/reference/plm.md)

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
#>               Intercept      B   Beta
#> Linear.ALL       41.820  4.077  0.964
#> Linear.A         54.085 -0.207 -0.056
#> Linear.B         55.277  4.891  0.982
#> Quadratic.ALL    54.644  0.210  0.976
#> Quadratic.A      53.733 -0.010 -0.011
#> Quadratic.B      67.383  0.327  0.953
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
#> Linear.ALL       41.892  2.576  0.945
#> Linear.A         56.248 -2.263 -0.932
#> Linear.B         53.670  2.720  0.939
#> Quadratic.ALL    52.077  0.117  0.932
#> Quadratic.A      53.533 -0.370 -0.933
#> Quadratic.B      62.523  0.156  0.887
#> Cubic.ALL        41.892  2.576  0.945
#> Cubic.A          56.248 -2.263 -0.932
#> Cubic.B          53.670  2.720  0.939
#> Log Time.ALL     34.174 16.422  0.828
#> Log Time.A       54.589 -5.358 -0.887
#> Log Time.B       47.218 15.167  0.916
#> 
#> Note. Measurement-times start at 1 for each phase
```

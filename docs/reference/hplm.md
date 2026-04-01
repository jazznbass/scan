# Hierarchical piecewise linear model / piecewise regression for multiple cases

The `hplm()` function computes a hierarchical piecewise regression
model. It extends the standard piecewise regression model to multiple
cases by estimating fixed and random effects. The function uses the
`lme` function of the `nlme` package to fit linear mixed-effects models.
The model can include random intercepts and random slopes for level,
trend, and treatment effects. Additionally, it allows for the inclusion
of autoregressive structures and unequal variances across phases. The
function also provides options for likelihood ratio tests to compare
models with and without random slope effects, as well as the calculation
of intraclass correlations (ICC) to assess the proportion of variance
attributable to between-case differences. This function is particularly
useful for analyzing data from multiple single-case experimental designs
(SCEDs) where observations are nested within cases.

## Usage

``` r
hplm(
  data,
  dvar,
  pvar,
  mvar,
  model = c("W", "H-M", "B&L-B", "JW"),
  contrast = c("first", "preceding"),
  contrast_level = NA,
  contrast_slope = NA,
  method = c("ML", "REML"),
  control = list(opt = "optim"),
  random.slopes = FALSE,
  lr.test = FALSE,
  ICC = TRUE,
  trend = TRUE,
  level = TRUE,
  slope = TRUE,
  random_trend = FALSE,
  random_level = FALSE,
  random_slope = FALSE,
  fixed = NULL,
  random = NULL,
  ar = 0,
  unequal_variances = FALSE,
  update.fixed = NULL,
  data.l2 = NULL,
  ...
)

# S3 method for class 'sc_hplm'
print(x, digits = 3, bcsmd = FALSE, casewise = FALSE, ...)

# S3 method for class 'sc_hplm'
export(
  object,
  caption = NA,
  footnote = NA,
  filename = NA,
  round = 2,
  nice = TRUE,
  casewise = FALSE,
  ...
)

# S3 method for class 'sc_hplm'
coef(object, casewise = FALSE, ...)
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

- model:

  Model used for calculating the dummy parameters (see Huitema & McKean,
  2000). Default is `model = "W"`. Possible values are: `"B&L-B"`,
  `"H-M"`, `"W"`, and deprecated `"JW"`.

- contrast:

  Sets contrast_level and contrast_slope. Either "first", "preceding" or
  a contrast matrix. If NA contrast is ignored.

- contrast_level:

  Either "first", "preceding" or a contrast matrix. If NA contrast_level
  is a copy of contrast.

- contrast_slope:

  Either "first", "preceding" or a contrast matrix. If NA contrast_level
  is a copy of contrast.

- method:

  Method used to fit your model. Pass `"REML"` to maximize the
  restricted log-likelihood or `"ML"` for maximized log-likelihood.
  Default is `"ML"`.

- control:

  A list of settings for the estimation algorithm, replacing the default
  values passed to the function `lmeControl` of the `nlme` package.

- random.slopes:

  If `random.slopes = TRUE` random slope effects of the level, trend,
  and treatment parameter are estimated.

- lr.test:

  If set TRUE likelihood ratio tests are calculated comparing model with
  vs. without random slope parameters.

- ICC:

  If `ICC = TRUE` an intraclass-correlation is estimated.

- trend:

  A logical indicating if a trend parameters is included in the model.

- level:

  A logical indicating if a level parameters is included in the model.

- slope:

  A logical indicating if a slope parameters is included in the model.

- random_trend:

  If TRUE, includes a random trend trend effect.

- random_level:

  If TRUE, includes a random level trend effect.

- random_slope:

  If TRUE, includes a random slope trend effect.

- fixed:

  Defaults to the fixed part of the standard piecewise regression model.
  The parameter phase followed by the phase name (e.g., phaseB)
  indicates the level effect of the corresponding phase. The parameter
  'inter' followed by the phase name (e.g., interB) adresses the slope
  effect based on the method provide in the model argument (e.g.,
  "B&L-B"). The formula can be changed for example to include further L1
  or L2 variables into the regression model.

- random:

  The random part of the model. Defaults to a random intercept model.
  The formula can be changed to include random slope effects for level,
  trend, and treatment effects.

- ar:

  Maximal lag of autoregression. Modelled based on the
  Autoregressive-Moving Average (ARMA) function.

- unequal_variances:

  Logical. If set TRUE, estimations are weighted by phase variances.

- update.fixed:

  An easier way to change the fixed model part (e.g.,
  `. ~ . + newvariable`).

- data.l2:

  A data frame providing additional variables at Level 2. The scdf File
  has to have names for all cases and the Level 2 data frame has to have
  a column named 'cases' with the names of the cases the Level 2
  variables belong to.

- ...:

  Further arguments passed to the lme function.

- x:

  An object returned by `hplm()`

- digits:

  The minimum number of significant digits to be use. If set to "auto"
  (default), values are predefined.

- bcsmd:

  If TRUE, reports between-case standardized mean differences.

- casewise:

  Returns the estimations for each case separately

- object:

  An scdf or an object exported from a scan function.

- caption:

  Character string with table caption. If left NA (default) a caption
  will be created based on the exported object.

- footnote:

  Character string with table footnote. If left NA (default) a footnote
  will be created based on the exported object.

- filename:

  String containing the file name. If a filename is given the output
  will be written to that file.

- round:

  Integer passed to the digits argument used to round values.

- nice:

  If set TRUE (default) output values are rounded and optimized for
  publication tables.

## Value

An object of class `sc_hplm`.

- `model` \| List containing infromation about

- `N` \| Number of single-cases.

- `formula` \| A list containing the fixed and the random formulas of
  the hplm model.

- `hplm` \| Object of class lme contaning the multilevel model.

- `model.0` \| Object of class lme containing the zero model.

- `ICC` \| List containing intraclass correlation and test parameters.

- `model.without` \| Object of class gls containing the fixed effect
  model.

- `contrast` \| List with contrast definitions.

## Functions

- `print(sc_hplm)`: Print results

- `export(sc_hplm)`: Export results as html table (see
  [`export()`](https://jazznbass.github.io/scan/reference/export.md))

- `coef(sc_hplm)`: Extract model coefficients

## Model specification

The fixed effects part of the model can be specified using the `fixed`
argument, while the random effects part can be specified using the
`random` argument. If not provided, default formulas based on the
specified model type (e.g., "B&L-B") are created. The function also
allows for the inclusion of autoregressive structures through the `ar`
argument and unequal variances across phases through the
`unequal_variances` argument.

## Random slopes

By setting the `random.slopes` argument to TRUE, the model will include
random slope effects for level, trend, and treatment effects. This
allows for individual differences in how cases respond to these effects.

## Likelihood ratio tests

If the `lr.test` argument is set to TRUE, the function will perform
likelihood ratio tests to compare models with and without random slope
effects. This helps to determine whether including random slopes
significantly improves model fit.

## Intraclass correlation

If the `ICC` argument is set to TRUE, the function will calculate the
intraclass correlation coefficient (ICC) to assess the proportion of
variance attributable to between-case differences. This provides insight
into the degree of similarity among observations within the same case.

## See also

Other regression functions:
[`bplm()`](https://jazznbass.github.io/scan/reference/bplm.md),
[`fetch()`](https://jazznbass.github.io/scan/reference/fetch.md),
[`mplm()`](https://jazznbass.github.io/scan/reference/mplm.md),
[`plm()`](https://jazznbass.github.io/scan/reference/plm.md),
[`print.sc_ac()`](https://jazznbass.github.io/scan/reference/autocorr.md),
[`print.sc_bctau()`](https://jazznbass.github.io/scan/reference/corrected_tau.md),
[`trend()`](https://jazznbass.github.io/scan/reference/trend.md)

## Author

Juergen Wilbert

## Examples

``` r
## Compute hplm model on a MBD over fifty cases (restricted log-likelihood)
hplm(exampleAB_50, method = "REML", random.slopes = FALSE)
#> Hierarchical Piecewise Linear Regression
#> 
#> Estimation method REML 
#> Contrast model: W / level: first, slope: first
#> 50 Cases
#> 
#> AIC = 8764.5, BIC = 8795.866
#> ICC = 0.292; L = 341.2; p = 0.000 
#> 
#> Fixed effects (values ~ 1 + mt + phaseB + interB)
#> 
#>                             B    SE   df      t p
#> Intercept              48.398 1.496 1328 32.351 0
#> Trend (mt)              0.579 0.116 1328  5.007 0
#> Level phase B (phaseB) 14.038 0.655 1328 21.442 0
#> Slope phase B (interB)  0.902 0.119 1328  7.589 0
#> 
#> Random effects (~1 | case)
#> 
#>               SD
#> Intercept 10.073
#> Residual   5.290

## Analyzing with additional L2 variables
Leidig2018 |>
  add_l2(Leidig2018_l2) |>
  hplm(update.fixed = .~. + gender + migration + ITRF_TOTAL*phaseB,
       slope = FALSE, random.slopes = FALSE, lr.test = FALSE
  )
#> Hierarchical Piecewise Linear Regression
#> 
#> Estimation method ML 
#> Contrast model: W / level: first, slope: first
#> 35 Cases
#> 
#> AIC = 5827.167, BIC = 5879.268
#> ICC = 0.344; L = 875.4; p = 0.000 
#> 
#> Fixed effects (academic_engagement ~ mt + phaseB + gender + migration + ITRF_TOTAL +     phaseB:ITRF_TOTAL)
#> 
#>                                        B    SE   df      t     p
#> Intercept                          3.751 0.262 2376 14.302 0.000
#> Trend (mt)                         0.004 0.001 2376  6.019 0.000
#> Level phase B (phaseB)             0.667 0.098 2376  6.808 0.000
#> gender                            -0.020 0.301   31 -0.067 0.947
#> migration                         -0.300 0.193   31 -1.556 0.130
#> ITRF_TOTAL                        -0.035 0.013   31 -2.674 0.012
#> Level phase B (phaseB):ITRF_TOTAL -0.001 0.005 2376 -0.279 0.780
#> 
#> Random effects (~1 | case)
#> 
#>              SD
#> Intercept 0.557
#> Residual  0.785
```

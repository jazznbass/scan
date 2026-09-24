# Piecewise linear model / piecewise regression

The `plm` function computes a piecewise regression model (see Huitema &
McKean, 2000) for one case. The function automatically creates the
default model which includes trend, level, and slope effects. The model
can be changed with `trend`, `level`, and `slope` arguments or by
providing a custom formula.

## Usage

``` r
plm(
  data,
  dvar,
  pvar,
  mvar,
  AR = 0,
  model = c("W", "H-M", "B&L-B", "JW"),
  family = "gaussian",
  trend = TRUE,
  level = TRUE,
  slope = TRUE,
  contrast = c("first", "preceding"),
  contrast_level = c(NA, "first", "preceding"),
  contrast_slope = c(NA, "first", "preceding"),
  formula = NULL,
  update = NULL,
  na.action = na.omit,
  r_squared = TRUE,
  var_trials = NULL,
  dvar_percentage = FALSE,
  ...
)

# S3 method for class 'sc_plm'
print(
  x,
  lag_max = 3,
  ci = 0.95,
  q = FALSE,
  r_squared = getOption("scan.rsquared"),
  ...
)

# S3 method for class 'sc_plm'
export(
  object,
  caption = NA,
  footnote = NA,
  filename = NA,
  nice = TRUE,
  ci = 0.95,
  q = FALSE,
  round = 2,
  r_squared = getOption("scan.rsquared"),
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

- AR:

  Maximal lag of autoregression. Modelled based on the
  Autoregressive-Moving Average (ARMA) function. When AR is set, the
  family argument must be set to `family = "gaussian"`. The
  autocorrelation is modelled along the measurement-time variable, so
  its values must be whole numbers and unique within a case. Otherwise a
  warning is given and `AR` is set to `0`.

- model:

  Model used for calculating the dummy parameters (see Huitema & McKean,
  2000). Default is `model = "W"`. Possible values are: `"B&L-B"`,
  `"H-M"`, `"W"`, and deprecated `"JW"`.

- family:

  Set the distribution family. Defaults to a gaussian distribution. See
  the `family` function for more details.

- trend:

  A logical indicating if a trend parameters is included in the model.

- level:

  A logical indicating if a level parameters is included in the model.

- slope:

  A logical indicating if a slope parameters is included in the model.

- contrast:

  Sets contrast_level and contrast_slope. Either "first", "preceding" or
  a contrast matrix. If NA contrast is ignored.

- contrast_level:

  Either "first", "preceding" or a contrast matrix. If NA contrast_level
  is a copy of contrast.

- contrast_slope:

  Either "first", "preceding" or a contrast matrix. If NA contrast_level
  is a copy of contrast.

- formula:

  Defaults to the standard piecewise regression model. The parameter
  phase followed by the phase name (e.g., phaseB) indicates the level
  effect of the corresponding phase. The parameter 'inter' followed by
  the phase name (e.g., interB) adresses the slope effect based on the
  method provide in the model argument (e.g., "B&L-B"). The formula can
  be changed for example to include further variables into the
  regression model.

- update:

  An easier way to change the regression formula (e.g.,
  `. ~ . + newvariable`).

- na.action:

  Defines how to deal with missing values.

- r_squared:

  Either "delta", "partial", or "none".

- var_trials:

  Name of the variable containing the number of trials (only for
  binomial regressions). If a single integer is provided this is
  considered to be a the constant number of trials across all
  measurements.

- dvar_percentage:

  Only for binomial distribution. If set TRUE, the dependent variable is
  assumed to represent proportions `[0,1]`. Otherwise dvar is assumed to
  represent counts.

- ...:

  Further arguments passed to the
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html) or
  [`nlme::gls()`](https://rdrr.io/pkg/nlme/man/gls.html) function.

- x:

  Object

- lag_max:

  Maximum lag to be reported for autocorrelation of residuals. Default
  is `3`. Set `FALSE` for no report of autocorrelations.

- ci:

  Print confidence intervals. Either FALSE, TRUE or a number between 0
  and 1 (0.90 for a 90% intervals).

- q:

  Logical. If set `TRUE`, Yule's Q is reported.

- object:

  An scdf or an object exported from a scan function.

- caption:

  Character string with table caption. If left NA (default) a caption
  will be created based on the exported object.

- footnote:

  Character string with table footnote. Several strings are combined
  into a footnote of several lines. If left NA (default) a footnote will
  be created based on the exported object. `NULL` or `""` suppress the
  footnote.

- filename:

  String containing the file name. If a filename is given the output
  will be written to that file.

- nice:

  If set TRUE (default) output values are rounded and optimized for
  publication tables.

- round:

  Integer passed to the digits argument used to round values.

## Value

An object of class `sc_plm`.

- formula:

  plm formula. Uselful if you want to use the update or formula argument
  and you don't know the names of the parameters.

- model:

  Character string from function call (see `Arguments` above).

- F.test:

  F-test values of modelfit.

- r.squares:

  Explained variance R squared for each model parameter.

- ar:

  Autoregression lag from function call (see `Arguments` above).

- family:

  Distribution family from function call (see `Arguments` above).

- contrast:

  List with the level and slope contrast definitions.

- var_trials:

  Number of trials from function call (see `Arguments` above), only for
  binomial regressions.

- dvar_percentage:

  Logical from function call (see `Arguments` above), only for binomial
  regressions.

- full.model:

  Full regression model list from the gls or glm function.

- data:

  The data the model was fitted to, including the dummy variables for
  level and slope effects. For a binomial regression with
  `dvar_percentage = FALSE` the dependent variable holds the proportions
  that were modelled, not the counts that were passed in.

## Details

The function uses the `glm` function of the stats package or the `gls`
function of the nlme package (for auto-regression models). For `AR > 0`
the model is estimated with generalized least squares. The F test, R
squared and the delta R squares are then computed in the metric of the
dependent variable, using the residuals of the fitted model. They are
descriptive in that case: the sums of squares do not decompose exactly,
and the F statistic is not exactly F distributed. With `AR > 0`
individual delta R squares can become negative, because the sums of
squares do not decompose exactly in this metric.

## Functions

- `print(sc_plm)`: Print

- `export(sc_plm)`: Export results as html table (see
  [`export()`](https://jazznbass.github.io/scan/reference/export.md))

## References

Beretvas, S., & Chung, H. (2008). An evaluation of modified R2-change
effect size indices for single-subject experimental designs.
*Evidence-Based Communication Assessment and Intervention, 2*, 120-128.

Huitema, B. E., & McKean, J. W. (2000). Design specification issues in
time-series intervention models. *Educational and Psychological
Measurement, 60*, 38-58.

## See also

Other regression functions:
[`autocorr()`](https://jazznbass.github.io/scan/reference/autocorr.md),
[`bplm()`](https://jazznbass.github.io/scan/reference/bplm.md),
[`corrected_tau()`](https://jazznbass.github.io/scan/reference/corrected_tau.md),
[`fetch()`](https://jazznbass.github.io/scan/reference/fetch.md),
[`hplm()`](https://jazznbass.github.io/scan/reference/hplm.md),
[`mplm()`](https://jazznbass.github.io/scan/reference/mplm.md),
[`trend()`](https://jazznbass.github.io/scan/reference/trend.md)

## Author

Juergen Wilbert

## Examples

``` r

## Compute a piecewise regression model for a random single-case
set.seed(123)
AB <- design(
  phase_design = list(A = 10, B = 20),
  level = list(A = 0, B = 1), slope = list(A = 0, B = 0.05),
  trend = 0.05
)
dat <- random_scdf(design = AB)
plm(dat, AR = 3)
#> Piecewise Regression Analysis
#> 
#> Contrast model: W / level = first, slope = first
#> 
#> Fitted a gaussian distribution.
#> Autocorrelated residuals up to lag 3 were modeled
#> 
#> F(3, 26) = 47.38; p <.001; R² = 0.845; Adjusted R² = 0.828; AIC = 185.8
#> 
#>                             B LL-CI95% UL-CI95%    SE      t     p delta R²
#> Intercept              53.068   48.188   57.948 2.490 21.315 0.000         
#> Trend (mt)             -0.166   -1.032    0.701 0.442 -0.374 0.711   -0.001
#> Level phase B (phaseB) 16.266   10.061   22.470 3.166  5.138 0.000    0.069
#> Slope phase B (interB)  0.913   -0.045    1.871 0.489  1.869 0.073    0.018
#> 
#> Autocorrelations of the residuals
#>  lag    cr
#>    1 -0.12
#>    2 -0.23
#>    3  0.38
#> Ljung-Box test: X²(3) = 7.32; p = .06 
#> 
#> Formula: values ~ 1 + mt + phaseB + interB 

## Another example with a more complex design
A1B1A2B2 <- design(
  phase_design = list(A1 = 15, B1 = 20, A2 = 15, B2 = 20),
  level = list(A1 = 0, B1 = 1, A2 = -1, B2 = 1),
  slope = list(A1 = 0, B1 = 0.0, A2 = 0, B2 = 0.0),
  trend = 0.0)
dat <- random_scdf(design = A1B1A2B2, seed = 123)
plm(dat, contrast = "preceding")
#> Piecewise Regression Analysis
#> 
#> Contrast model: W / level = preceding, slope = preceding
#> 
#> Fitted a gaussian distribution.
#> F(7, 62) = 11.05; p <.001; R² = 0.555; Adjusted R² = 0.505; AIC = 425
#> 
#>                                B LL-CI95% UL-CI95%    SE      t     p delta R²
#> Intercept                 51.354   46.820   55.889 2.314 22.196 0.000         
#> Trend (mt)                -0.085   -0.636    0.467 0.281 -0.301 0.764    0.001
#> Level phase B1 (phaseB1)   7.816    1.418   14.213 3.264  2.395 0.020    0.041
#> Level phase A2 (phaseA2) -11.360  -17.599   -5.121 3.183 -3.569 0.001    0.091
#> Level phase B2 (phaseB2)  10.380    3.983   16.777 3.264  3.180 0.002    0.073
#> Slope phase B1 (interB1)   0.280   -0.377    0.937 0.335  0.835 0.407    0.005
#> Slope phase A2 (interA2)  -0.240   -0.897    0.417 0.335 -0.716 0.477    0.004
#> Slope phase B2 (interB2)   0.119   -0.538    0.776 0.335  0.354 0.724    0.001
#> 
#> Autocorrelations of the residuals
#>  lag    cr
#>    1 -0.05
#>    2 -0.12
#>    3  0.12
#> Ljung-Box test: X²(3) = 2.27; p = .51 
#> 
#> Formula: values ~ 1 + mt + phaseB1 + phaseA2 + phaseB2 + interB1 + interA2 +      interB2 

## no slope effects were found. Therefore, you might want to the drop slope
## estimation:
plm(dat, slope = FALSE, contrast = "preceding")
#> Piecewise Regression Analysis
#> 
#> Contrast model: W / level = preceding, slope = preceding
#> 
#> Fitted a gaussian distribution.
#> F(4, 65) = 19.73; p <.001; R² = 0.548; Adjusted R² = 0.521; AIC = 420.1
#> 
#>                                B LL-CI95% UL-CI95%    SE      t     p delta R²
#> Intercept                 50.232   47.470   52.994 1.409 35.645 0.000         
#> Trend (mt)                 0.076   -0.133    0.284 0.107  0.710 0.480    0.004
#> Level phase B1 (phaseB1)   7.671    2.879   12.463 2.445  3.137 0.003    0.068
#> Level phase A2 (phaseA2) -10.945  -15.737   -6.153 2.445 -4.477 0.000    0.139
#> Level phase B2 (phaseB2)   9.402    4.610   14.194 2.445  3.846 0.000    0.103
#> 
#> Autocorrelations of the residuals
#>  lag    cr
#>    1 -0.03
#>    2 -0.09
#>    3  0.13
#> Ljung-Box test: X²(3) = 2.02; p = .56 
#> 
#> Formula: values ~ 1 + mt + phaseB1 + phaseA2 + phaseB2 

## and now drop the trend estimation as well
plm(dat, slope = FALSE, trend = FALSE, contrast = "preceding")
#> Piecewise Regression Analysis
#> 
#> Contrast model: W / level = preceding, slope = preceding
#> 
#> Fitted a gaussian distribution.
#> F(3, 66) = 26.34; p <.001; R² = 0.545; Adjusted R² = 0.524; AIC = 418.6
#> 
#>                               B LL-CI95% UL-CI95%    SE      t p delta R²
#> Intercept                50.762   48.427   53.097 1.191 42.611 0         
#> Level phase B1 (phaseB1)  8.995    5.906   12.084 1.576  5.708 0    0.225
#> Level phase A2 (phaseA2) -9.621  -12.710   -6.532 1.576 -6.105 0    0.257
#> Level phase B2 (phaseB2) 10.726    7.637   13.815 1.576  6.806 0    0.319
#> 
#> Autocorrelations of the residuals
#>  lag    cr
#>    1 -0.01
#>    2 -0.08
#>    3  0.13
#> Ljung-Box test: X²(3) = 1.82; p = .61 
#> 
#> Formula: values ~ 1 + phaseB1 + phaseA2 + phaseB2 

## A poisson regression
example_A24 |>
  plm(family = "poisson")
#> Piecewise Regression Analysis
#> 
#> Contrast model: W / level = first, slope = first
#> 
#> Fitted a poisson distribution.
#> X²(3) = 547.67; p <.001; AIC = 261.3
#> 
#>                             B LL-CI95% UL-CI95%    SE       t     p      OR
#> Intercept               5.556    5.472    5.638 0.042 131.693 0.000 258.786
#> Trend (year)            0.007   -0.016    0.030 0.012   0.604 0.546   1.007
#> Level phase B (phaseB) -0.806   -0.938   -0.674 0.067 -11.964 0.000   0.447
#> Slope phase B (interB) -0.006   -0.031    0.019 0.013  -0.472 0.637   0.994
#>                         LL-CI95%  UL-CI95%
#> Intercept                237.936   280.900
#> Trend (year)               0.984     1.030
#> Level phase B (phaseB)     0.391     0.510
#> Slope phase B (interB)     0.969     1.019
#> 
#> Formula: injuries ~ 1 + year + phaseB + interB 
#> 
#> The following variables were used in this analysis:
#> 'injuries' as dependent variable, 'phase' as phase variable, and 'year' as measurement-time variable.

## A binomial regression (frequencies as dependent variable)
plm(exampleAB_score$Christiano, family = "binomial", var_trials = "trials")
#> Piecewise Regression Analysis
#> 
#> Contrast model: W / level = first, slope = first
#> 
#> Fitted a binomial distribution.
#> X²(3) = 240.66; p <.001; AIC = 120.3
#> 
#>                             B LL-CI95% UL-CI95%    SE      t     p     OR
#> Intercept              -1.964   -2.793   -1.239 0.394 -4.991 0.000  0.140
#> Trend (mt)              0.023   -0.118    0.166 0.072  0.324 0.746  1.023
#> Level phase B (phaseB)  2.376    1.454    3.378 0.488  4.866 0.000 10.762
#> Slope phase B (interB)  0.038   -0.111    0.186 0.075  0.504 0.614  1.039
#>                         LL-CI95%  UL-CI95%
#> Intercept                  0.061     0.290
#> Trend (mt)                 0.889     1.181
#> Level phase B (phaseB)     4.280    29.312
#> Slope phase B (interB)     0.895     1.204
#> 
#> Formula: values/trials ~ 1 + mt + phaseB + interB
#> weights = trials 

## A binomial regression (percentage as dependent variable)
exampleAB_score$Christiano |>
  transform(percentage = values/trials) |>
  set_dvar("percentage") |>
  plm(family = "binomial", var_trials = "trials", dvar_percentage = TRUE)
#> Piecewise Regression Analysis
#> 
#> Contrast model: W / level = first, slope = first
#> 
#> Fitted a binomial distribution.
#> X²(3) = 240.66; p <.001; AIC = 120.3
#> 
#>                             B LL-CI95% UL-CI95%    SE      t     p     OR
#> Intercept              -1.964   -2.793   -1.239 0.394 -4.991 0.000  0.140
#> Trend (mt)              0.023   -0.118    0.166 0.072  0.324 0.746  1.023
#> Level phase B (phaseB)  2.376    1.454    3.378 0.488  4.866 0.000 10.762
#> Slope phase B (interB)  0.038   -0.111    0.186 0.075  0.504 0.614  1.039
#>                         LL-CI95%  UL-CI95%
#> Intercept                  0.061     0.290
#> Trend (mt)                 0.889     1.181
#> Level phase B (phaseB)     4.280    29.312
#> Slope phase B (interB)     0.895     1.204
#> 
#> Formula: percentage ~ 1 + mt + phaseB + interB
#> weights = trials 
#> 
#> The following variables were used in this analysis:
#> 'percentage' as dependent variable, 'phase' as phase variable, and 'mt' as measurement-time variable.
## Print
plm(exampleAB$Johanna) |> 
  print(ci = 0.9, r_squared = c("delta", "partial"))
#> Piecewise Regression Analysis
#> 
#> Contrast model: W / level = first, slope = first
#> 
#> Fitted a gaussian distribution.
#> F(3, 16) = 28.69; p <.001; R² = 0.843; Adjusted R² = 0.814; AIC = 126.8
#> 
#>                             B LL-CI90% UL-CI90%    SE      t     p delta R²
#> Intercept              54.400   48.002   60.798 3.890 13.986 0.000         
#> Trend (mt)              0.100   -2.512    2.712 1.588  0.063 0.951    0.000
#> Level phase B (phaseB)  7.858   -1.709   17.426 5.816  1.351 0.195    0.018
#> Slope phase B (interB)  1.525   -1.133    4.183 1.616  0.944 0.359    0.009
#>                        partial R²
#> Intercept                        
#> Trend (mt)                  0.000
#> Level phase B (phaseB)      0.102
#> Slope phase B (interB)      0.053
#> 
#> Autocorrelations of the residuals
#>  lag    cr
#>    1 -0.32
#>    2 -0.13
#>    3 -0.01
#> Ljung-Box test: X²(3) = 2.84; p = .41 
#> 
#> Formula: values ~ 1 + mt + phaseB + interB 
## Export
plm(exampleAB$Johanna) |> export()


  


Piecewise-regression model predicting ‘values’
```

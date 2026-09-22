# Estimate single-case design

This functions takes an scdf and extracts design parameters. The
resulting object can be used to randomly create new scdf files with the
same underlying parameters. This is useful for Monte-Carlo studies and
bootstrapping procedures.

## Usage

``` r
estimate_design(
  data,
  dvar,
  pvar,
  mvar,
  s = NULL,
  rtt = NULL,
  error = NULL,
  overall_effects = FALSE,
  overall_rtt = TRUE,
  model = "JW",
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

- s:

  The standard deviation depicting the between case variance of the
  overall performance. If more than two single-cases are included in the
  scdf, the variance is estimated if s is set to NULL. The estimate is
  the variance of the estimated start values reduced by the mean squared
  standard error of these estimates, so that the uncertainty of the
  single estimates does not inflate `s`. When that difference is not
  positive, `s` is set equal to the standard deviation of the error and
  a warning is issued (see details). If s is provided, this value is
  used.

- rtt:

  The reliability of the measurements. The reliability is estimated when
  rtt = NULL, as `s^2 / (s^2 + var_error)`, the definition
  [`design()`](https://jazznbass.github.io/scan/reference/design.md)
  uses to draw the measurement error. `var_error` is the residual
  variance of the piecewise regression, pooled across cases when
  `overall_rtt = TRUE`. If rtt is provided, this value is used for all
  single-cases.

- error:

  Standard deviation of the measurement error. An alternative to `rtt`:
  the reliability of each case is then derived as
  `s^2 / (s^2 + error^2)`. `rtt` and `error` must not be given together.
  To assign different values to several single-cases, use a vector of
  values.

- overall_effects:

  If TRUE, trend, level, and slope effect estimations will be identical
  for each case. If FALSE, effects are estimated for each case
  separately. Default is FALSE.

- overall_rtt:

  Ignored when `rtt` or `error` is set. If TRUE, rtt estimations will be
  based on all cases and identical for each case. If FALSE rtt is
  estimated for each case separately. Default is TRUE.

- model:

  Model used for calculating the dummy parameters (see Huitema & McKean,
  2000). Default is `model = "W"`. Possible values are: `"B&L-B"`,
  `"H-M"`, `"W"`, and deprecated `"JW"`.

- ...:

  Further arguments passed to the plm function used for parameter
  estimation. For example, the model argument can be used to select
  different piecewise regression models. See plm for details.

## Value

A list of parameters for each single-case. Parameters include name,
length, and starting measurement time of each phase, trend, level, and
slope effects for each phase, start value, standard deviation, and
reliability for each case. This list can be used as input for the
random_scdf function to create new random scdf files based on the
estimated parameters.

## Details

The function uses the plm function to estimate parameters for each
single-case. If more than two single-cases are included in the scdf, the
between case variance depicting the overall performance (s) is estimated
unless s is provided. The reliability of the measurements (rtt) is
estimated for each case unless rtt is provided. If overall_rtt is set to
TRUE, rtt estimations will be based on all cases and identical for each
case. If overall_effects is set to TRUE, trend, level, and slope effect
estimations will be identical for each case.

`s` is estimated from the differences between the start values of the
cases, so it can only be estimated when the cases differ in their
starting level by more than the precision of these estimates — with one
or two cases it can not be estimated at all. When it can not, `s` is set
equal to the standard deviation of the error and a warning is issued:
the effects are then expressed in units of the variation within a case,
and the reliability is fixed at 0.5 by that choice rather than
estimated. Providing `s`, `rtt` or `error` is strongly recommended for
such data. `s` and `rtt` are two sides of one parameter: whichever `s`
is used, the error distribution derived from the estimated `rtt` has the
residual variance of the piecewise regression, and the effects are
stored in units of `s` and multiplied by `s` again when data are
simulated. A different `s` therefore changes the reported parameters,
not the simulated data.

The resulting design object can be used as input for the random_scdf
function to create new random scdf files based on the estimated
parameters. This allows to create bootstrap samples or Monte-Carlo
datasets based on the characteristics of an existing dataset.

## Author

Juergen Wilbert

## Examples

``` r
# create a random scdf with predefined parameters
set.seed(1234)
design <- design(
  n = 10, trend = -0.02,
  level = list(0, 1), rtt = 0.8,
  s = 1, random_start_value = TRUE
)
scdf<- random_scdf(design)

# Estimate the parameters based on the scdf and create a new random scdf
# based on these estimations
design_est <- estimate_design(scdf, rtt = 0.8)
scdf_est <- random_scdf(design_est)

# Analyze both datasets with an hplm model. See how similar the estimations
# are:
hplm(scdf, slope = FALSE)
#> Hierarchical Piecewise Linear Regression
#> 
#> Estimation method ML 
#> Contrast model: W / level: first, slope: first
#> 10 Cases
#> 
#> AIC = 342.6379, BIC = 359.1295
#> ICC = 0.697; L = 200.3; p <.001 
#> 
#> Fixed effects (values ~ 1 + mt + phaseB)
#> 
#>                             B    SE  df       t     p
#> Intercept              49.537 0.315 188 157.279 0.000
#> Trend (mt)             -0.009 0.009 188  -0.913 0.362
#> Level phase B (phaseB)  0.952 0.124 188   7.659 0.000
#> 
#> Random effects (~1 | case)
#> 
#>              SD
#> Intercept 0.961
#> Residual  0.499
hplm(scdf_est, slope = FALSE)
#> Hierarchical Piecewise Linear Regression
#> 
#> Estimation method ML 
#> Contrast model: W / level: first, slope: first
#> 10 Cases
#> 
#> AIC = 449.8999, BIC = 466.3915
#> ICC = 0.629; L = 162.9; p <.001 
#> 
#> Fixed effects (values ~ 1 + mt + phaseB)
#> 
#>                             B    SE  df       t     p
#> Intercept              49.502 0.337 188 147.075 0.000
#> Trend (mt)             -0.008 0.012 188  -0.657 0.512
#> Level phase B (phaseB)  0.986 0.164 188   5.999 0.000
#> 
#> Random effects (~1 | case)
#> 
#>              SD
#> Intercept 1.011
#> Residual  0.660

# Also similar results for pand and randomization tests:
pand(scdf)
#> Percentage of all non-overlapping data
#> 
#> Method: sort 
#> 
#> PAND = 87%
#> Φ =  0.653  ; Φ² =  0.427 
#> 
#> 200 measurements (50 Phase A, 150 Phase B) in 10 cases
#> Overlapping data: n = 26 ; percentage = 13 
#> 
#> 2 x 2 Matrix of percentages
#>          A    B total
#> A     18.5  6.5    25
#> B      6.5 68.5    75
#> total 25.0 75.0   100
#> 
#> 2 x 2 Matrix of counts
#>        A   B total
#> A     37  13    50
#> B     13 137   150
#> total 50 150   200
#> 
#> 
#> Chi-Squared test:
#> X² = 85.369, df = 1, p <.001 
#> 
#> Fisher exact test:
#> Odds ratio = 29.095, p <.001 
pand(scdf_est)
#> Percentage of all non-overlapping data
#> 
#> Method: sort 
#> 
#> PAND = 81%
#> Φ =  0.493  ; Φ² =  0.243 
#> 
#> 200 measurements (50 Phase A, 150 Phase B) in 10 cases
#> Overlapping data: n = 38 ; percentage = 19 
#> 
#> 2 x 2 Matrix of percentages
#>          A    B total
#> A     15.5  9.5    25
#> B      9.5 65.5    75
#> total 25.0 75.0   100
#> 
#> 2 x 2 Matrix of counts
#>        A   B total
#> A     31  19    50
#> B     19 131   150
#> total 50 150   200
#> 
#> 
#> Chi-Squared test:
#> X² = 48.676, df = 1, p <.001 
#> 
#> Fisher exact test:
#> Odds ratio = 11.057, p <.001 
rand_test(scdf)
#> Randomization Test
#> 
#> Combined test for 10 cases.
#> 
#> Comparing phase 1 against phase 2 
#> Statistic:  Mean B-A 
#> 
#> Minimal length of each phase: A = 5 , B = 5 
#> Observed statistic =  0.8667941 
#> 
#> Distribution based on a random sample of all 25937424601 possible combinations.
#> n   =  500 
#> M   =  0.456899 
#> SD  =  0.07502394 
#> Min =  0.2555112 
#> Max =  0.6770502 
#> 
#> Probability of an equal or higher value than the observed statistic:
#> p   <  0.002 
#> 
#> Shapiro-Wilk Normality Test: W = 0.998; p = .83  (Hypothesis of normality maintained)
#> 
#> Probabilty of observed statistic based on the assumption of normality:
#> z = 5.4635, p = 0.0000 (single sided)
rand_test(scdf_est)
#> Randomization Test
#> 
#> Combined test for 10 cases.
#> 
#> Comparing phase 1 against phase 2 
#> Statistic:  Mean B-A 
#> 
#> Minimal length of each phase: A = 5 , B = 5 
#> Observed statistic =  0.9050207 
#> 
#> Distribution based on a random sample of all 25937424601 possible combinations.
#> n   =  500 
#> M   =  0.4697581 
#> SD  =  0.08220316 
#> Min =  0.2721832 
#> Max =  0.7589233 
#> 
#> Probability of an equal or higher value than the observed statistic:
#> p   <  0.002 
#> 
#> Shapiro-Wilk Normality Test: W = 0.991; p <.01  (Hypothesis of normality rejected)
#> 
#> Probabilty of observed statistic based on the assumption of normality:
#> z = 5.2950, p = 0.0000 (single sided)
```

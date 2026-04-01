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
  scdf, the variance is estimated if s is set to NULL. If s is provided,
  this value is used.

- rtt:

  The reliability of the measurements. The reliability is estimated when
  rtt = NULL. If rtt is provided, this value is used for all
  single-cases.

- overall_effects:

  If TRUE, trend, level, and slope effect estimations will be identical
  for each case. If FALSE, effects are estimated for each case
  separately. Default is FALSE.

- overall_rtt:

  Ignored when `rtt` is set. If TRUE, rtt estimations will be based on
  all cases and identical for each case. If FALSE rtt is estimated for
  each case separately. Default is TRUE.

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
  s = 1
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
#> AIC = 305.8896, BIC = 322.3812
#> ICC = 0.004; L = 0.0; p = 0.870 
#> 
#> Fixed effects (values ~ 1 + mt + phaseB)
#> 
#>                             B    SE  df       t     p
#> Intercept              50.004 0.079 188 633.489 0.000
#> Trend (mt)             -0.024 0.009 188  -2.579 0.011
#> Level phase B (phaseB)  1.010 0.125 188   8.088 0.000
#> 
#> Random effects (~1 | case)
#> 
#>              SD
#> Intercept 0.088
#> Residual  0.501
hplm(scdf_est, slope = FALSE)
#> Hierarchical Piecewise Linear Regression
#> 
#> Estimation method ML 
#> Contrast model: W / level: first, slope: first
#> 10 Cases
#> 
#> AIC = 424.4159, BIC = 440.9075
#> ICC = 0.071; L = 5.4; p = 0.020 
#> 
#> Fixed effects (values ~ 1 + mt + phaseB)
#> 
#>                             B    SE  df       t     p
#> Intercept              50.068 0.120 188 416.805 0.000
#> Trend (mt)             -0.028 0.012 188  -2.254 0.025
#> Level phase B (phaseB)  1.011 0.165 188   6.125 0.000
#> 
#> Random effects (~1 | case)
#> 
#>              SD
#> Intercept 0.220
#> Residual  0.662

# Also similar results for pand and randomization tests:
pand(scdf)
#> Percentage of all non-overlapping data
#> 
#> Method: sort 
#> 
#> PAND = 84%
#> Φ =  0.573  ; Φ² =  0.329 
#> 
#> 200 measurements (50 Phase A, 150 Phase B) in 10 cases
#> Overlapping data: n = 32 ; percentage = 16 
#> 
#> 2 x 2 Matrix of percentages
#>        A  B total
#> A     17  8    25
#> B      8 67    75
#> total 25 75   100
#> 
#> 2 x 2 Matrix of counts
#>        A   B total
#> A     34  16    50
#> B     16 134   150
#> total 50 150   200
#> 
#> 
#> Chi-Squared test:
#> X² = 65.742, df = 1, p = 0.000 
#> 
#> Fisher exact test:
#> Odds ratio = 17.393, p = 0.000 
pand(scdf_est)
#> Percentage of all non-overlapping data
#> 
#> Method: sort 
#> 
#> PAND = 79%
#> Φ =  0.44  ; Φ² =  0.194 
#> 
#> 200 measurements (50 Phase A, 150 Phase B) in 10 cases
#> Overlapping data: n = 42 ; percentage = 21 
#> 
#> 2 x 2 Matrix of percentages
#>          A    B total
#> A     14.5 10.5    25
#> B     10.5 64.5    75
#> total 25.0 75.0   100
#> 
#> 2 x 2 Matrix of counts
#>        A   B total
#> A     29  21    50
#> B     21 129   150
#> total 50 150   200
#> 
#> 
#> Chi-Squared test:
#> X² = 38.720, df = 1, p = 0.000 
#> 
#> Fisher exact test:
#> Odds ratio = 8.360, p = 0.000 
rand_test(scdf)
#> Randomization Test
#> 
#> Combined test for 10 cases.
#> 
#> Comparing phase 1 against phase 2 
#> Statistic:  Mean B-A 
#> 
#> Minimal length of each phase: A = 5 , B = 5 
#> Observed statistic =  0.7679039 
#> 
#> Distribution based on a random sample of all 25937424601 possible combinations.
#> n   =  500 
#> M   =  0.3085513 
#> SD  =  0.06906004 
#> Min =  0.1427584 
#> Max =  0.5118563 
#> 
#> Probability of an equal or higher value than the observed statistic:
#> p   <  0.002 
#> 
#> Shapiro-Wilk Normality Test: W = 0.994; p = 0.037  (Hypothesis of normality rejected)
#> 
#> Probabilty of observed statistic based on the assumption of normality:
#> z = 6.6515, p = 0.0000 (single sided)
rand_test(scdf_est)
#> Randomization Test
#> 
#> Combined test for 10 cases.
#> 
#> Comparing phase 1 against phase 2 
#> Statistic:  Mean B-A 
#> 
#> Minimal length of each phase: A = 5 , B = 5 
#> Observed statistic =  0.7314301 
#> 
#> Distribution based on a random sample of all 25937424601 possible combinations.
#> n   =  500 
#> M   =  0.269875 
#> SD  =  0.07922479 
#> Min =  0.08001321 
#> Max =  0.5456197 
#> 
#> Probability of an equal or higher value than the observed statistic:
#> p   <  0.002 
#> 
#> Shapiro-Wilk Normality Test: W = 0.992; p = 0.013  (Hypothesis of normality rejected)
#> 
#> Probabilty of observed statistic based on the assumption of normality:
#> z = 5.8259, p = 0.0000 (single sided)
```

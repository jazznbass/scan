# Bayesian Piecewise Linear Model (bplm)

Computes a bayesian (hierarchical) piecewise linear model based on a
Markov chain Monte Carlo sampler. The function automatically creates the
fixed and random part of the regression model.

## Usage

``` r
bplm(
  data,
  dvar,
  pvar,
  mvar,
  model = c("W", "H-M", "B&L-B"),
  contrast_level = c("first", "preceding"),
  contrast_slope = c("first", "preceding"),
  trend = TRUE,
  level = TRUE,
  slope = TRUE,
  random_trend = FALSE,
  random_level = FALSE,
  random_slope = FALSE,
  fixed = NULL,
  random = NULL,
  update_fixed = NULL,
  ...
)

# S3 method for class 'sc_bplm'
print(x, digits = 3, ...)

# S3 method for class 'sc_bplm'
export(
  object,
  caption = NA,
  footnote = NA,
  filename = NA,
  round = 2,
  nice = TRUE,
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

- model:

  Model used for calculating the dummy parameters (see Huitema & McKean,
  2000). Default is `model = "W"`. Possible values are: `"B&L-B"`,
  `"H-M"`, `"W"`, and deprecated `"JW"`.

- contrast_level:

  Either "first", "preceding" or a contrast matrix. If NA contrast_level
  is a copy of contrast.

- contrast_slope:

  Either "first", "preceding" or a contrast matrix. If NA contrast_level
  is a copy of contrast.

- trend:

  A logical indicating if a trend parameters is included in the model.

- level:

  A logical indicating if a level parameters is included in the model.

- slope:

  A logical indicating if a slope parameters is included in the model.

- random_trend:

  If TRUE, includes a random trend effect.

- random_level:

  If TRUE, includes a random level effect.

- random_slope:

  If TRUE, includes a random slope effect.

- fixed:

  A formula that overwrites the automatically created fixed part of the
  regression model that defaults to the standard piecewise regression
  model. The parameter phase followed by the phase name (e.g., phaseB)
  indicates the level effect of the corresponding phase. The parameter
  'inter' followed by the phase name (e.g., interB) adresses the slope
  effect based on the method provide in the model argument (e.g.,
  "B&L-B"). The formula can be changed for example to include further L1
  or L2 variables into the regression model.

- random:

  A formula that overwrites the automatically created random part of the
  regression model.

- update_fixed:

  An easier way to change the fixed model part (e.g.,
  `. ~ . + newvariable`).

- ...:

  Further arguments passed to the
  [`MCMCglmm::MCMCglmm()`](https://rdrr.io/pkg/MCMCglmm/man/MCMCglmm.html)
  function.

- x:

  An object returned by `bplm()`

- digits:

  The minimum number of significant digits to be use. If set to "auto"
  (default), values are predefined.

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

An object of class `sc_bplm` with element:

- `model` - List containing information about the applied model.

- `N` - Number of single-cases.

- `formula` - A list containing the fixed and the random formulas of the
  hplm model.

- `mcmglmm` - Object of class MCMglmm.

- `contrast` - List with contrast definitions.

## Details

The function uses the
[`MCMCglmm::MCMCglmm()`](https://rdrr.io/pkg/MCMCglmm/man/MCMCglmm.html)
function to fit the model. The default model includes fixed trend,
level, and slope effects as well as a random intercept for each
single-case. The fixed part of the model can be changed by providing a
custom formula to the `fixed` argument or by using the `update_fixed`
argument. The random part of the model can be changed by providing a
custom formula to the `random` argument or by setting the
`random_trend`, `random_level`, or `random_slope` arguments to TRUE.

## Functions

- `print(sc_bplm)`: Print results

- `export(sc_bplm)`: Export results as html table (see
  [`export()`](https://jazznbass.github.io/scan/reference/export.md))

## See also

Other regression functions:
[`fetch()`](https://jazznbass.github.io/scan/reference/fetch.md),
[`hplm()`](https://jazznbass.github.io/scan/reference/hplm.md),
[`mplm()`](https://jazznbass.github.io/scan/reference/mplm.md),
[`plm()`](https://jazznbass.github.io/scan/reference/plm.md),
[`print.sc_ac()`](https://jazznbass.github.io/scan/reference/autocorr.md),
[`print.sc_bctau()`](https://jazznbass.github.io/scan/reference/corrected_tau.md),
[`trend()`](https://jazznbass.github.io/scan/reference/trend.md)

## Author

Juergen Wilbert

## Examples

``` r
# plm regression
bplm(example_A24)
#> Bayesian Piecewise Linear Regression
#> 
#> Contrast model: W (level: first, slope: first)
#> Deviance Information Criterion: 221.87 
#> 
#> B-structure - Fixed effects (injuries ~ 1 + year + phaseB + interB)
#> 
#>                               B lower 95% CI upper 95% CI sample size     p
#> Intercept               258.493      225.124      300.038    1192.152 0.001
#> Trend (year)              2.032       -7.538       12.150    1000.000 0.666
#> Level phase B (phaseB) -150.934     -206.970     -104.339    1107.750 0.001
#> Slope phase B (interB)   -1.965      -11.167        9.533    1000.000 0.704
#> 
#> R-Structure - Residuals
#> 
#>           SD lower 95% CI upper 95% CI 
#>       28.059       19.024       37.313 

# Multilevel plm regression with random intercept
bplm(exampleAB_50, nitt = 5000)
#> Bayesian Piecewise Linear Regression
#> 
#> Contrast model: W (level: first, slope: first)
#> 50 Cases
#> 
#> Deviance Information Criterion: 8574.425 
#> 
#> B-structure - Fixed effects (values ~ 1 + mt + phaseB + interB)
#> 
#>                             B lower 95% CI upper 95% CI sample size     p
#> Intercept              48.444       45.747       51.379     200.000 0.005
#> Trend (mt)              0.586        0.365        0.799     200.000 0.005
#> Level phase B (phaseB) 14.004       12.886       15.080     198.408 0.005
#> Slope phase B (interB)  0.896        0.668        1.101     200.000 0.005
#> 
#> G-Structure - Random effects (~case)
#> 
#>  Parameter     SD lower 95% CI upper 95% CI
#>  Intercept 10.312        8.365       12.532
#> 
#> R-Structure - Residuals
#> 
#>           SD lower 95% CI upper 95% CI 
#>        5.293        5.069        5.488 

# Adding a random slope
bplm(exampleAB_50, random_level = TRUE, nitt = 5000)
#> Bayesian Piecewise Linear Regression
#> 
#> Contrast model: W (level: first, slope: first)
#> 50 Cases
#> 
#> Deviance Information Criterion: 8459.847 
#> 
#> B-structure - Fixed effects (values ~ 1 + mt + phaseB + interB)
#> 
#>                             B lower 95% CI upper 95% CI sample size     p
#> Intercept              48.249       45.233       50.582         200 0.005
#> Trend (mt)              0.635        0.441        0.899         200 0.005
#> Level phase B (phaseB) 13.813       11.856       15.178         200 0.005
#> Slope phase B (interB)  0.852        0.592        1.035         200 0.005
#> 
#> G-Structure - Random effects (~us(1 + phaseB):case)
#> 
#>               Parameter    SD lower 95% CI upper 95% CI
#>               Intercept 9.778        8.105       11.397
#>  Level phase B (phaseB) 3.985        2.900        5.059
#> 
#> Correlation
#>                         Parameter Correlation lower 95% CI upper 95% CI
#>  Intercept:Level phase B (phaseB)       0.086       -0.203        0.498
#> 
#> R-Structure - Residuals
#> 
#>           SD lower 95% CI upper 95% CI 
#>        5.017        4.811        5.177 

# Custom fixed formula
bplm(exampleAB_50, update_fixed = values ~ -1 + mt + phaseB +
  interB, nitt = 5000)
#> Bayesian Piecewise Linear Regression
#> 
#> Contrast model: W (level: first, slope: first)
#> 50 Cases
#> 
#> Deviance Information Criterion: 8576.374 
#> 
#> B-structure - Fixed effects (values ~ mt + phaseB + interB - 1)
#> 
#>                             B lower 95% CI upper 95% CI sample size     p
#> Trend (mt)              0.622        0.427        0.842         200 0.005
#> Level phase B (phaseB) 13.915       12.843       15.204         200 0.005
#> Slope phase B (interB)  0.861        0.625        1.067         200 0.005
#> 
#> G-Structure - Random effects (~case)
#> 
#>  Parameter     SD lower 95% CI upper 95% CI
#>  Intercept 50.051       39.044        58.57
#> 
#> R-Structure - Residuals
#> 
#>           SD lower 95% CI upper 95% CI 
#>        5.298        5.114        5.512 
```

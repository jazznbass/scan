# Multivariate Piecewise linear model / piecewise regression

The `mplm()` function computes a multivariate piecewise regression
model. The function automatically creates the regression formula based
on the provided data and the selected options. The default model
includes trend, level, and slope effects for each dependent variable.
The regression formula can be changed by providing a custom formula to
the `update` argument.

## Usage

``` r
mplm(
  data,
  dvar,
  mvar,
  pvar,
  model = c("W", "H-M", "B&L-B", "JW"),
  contrast = c("first", "preceding"),
  contrast_level = c(NA, "first", "preceding"),
  contrast_slope = c(NA, "first", "preceding"),
  trend = TRUE,
  level = TRUE,
  slope = TRUE,
  formula = NULL,
  update = NULL,
  na.action = na.omit,
  ...
)

# S3 method for class 'sc_mplm'
print(x, digits = "auto", std = FALSE, ...)

# S3 method for class 'sc_mplm'
export(
  object,
  caption = NA,
  footnote = NA,
  filename = NA,
  nice = TRUE,
  std = FALSE,
  decimals = 2,
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

- mvar:

  Character string with the name of the measurement time variable.
  Defaults to the attributes in the scdf file.

- pvar:

  Character string with the name of the phase variable. Defaults to the
  attributes in the scdf file.

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

- trend:

  A logical indicating if a trend parameters is included in the model.

- level:

  A logical indicating if a level parameters is included in the model.

- slope:

  A logical indicating if a slope parameters is included in the model.

- formula:

  Defaults to the standard piecewise regression model. The parameter
  phase followed by the phase name (e.g., `phaseB`) indicates the level
  effect of the corresponding phase. The parameter 'inter' followed by
  the phase name (e.g., `interB`) adresses the slope effect based on the
  method provide in the model argument (e.g., `"B&L-B"`). The formula
  can be changed for example to include further variables into the
  regression model.

- update:

  An easier way to change the regression formula (e.g.,
  `. ~ . + newvariable`).

- na.action:

  Defines how to deal with missing values.

- ...:

  Further arguments passed to the
  [`lm()`](https://rdrr.io/r/stats/lm.html) function.

- x:

  Object returned from `mplm()`.

- digits:

  The minimum number of significant digits to be use. If set to "auto"
  (default), values are predefined.

- std:

  If TRUE, a table with standardized estimates is included.

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

- nice:

  If set TRUE (default) output values are rounded and optimized for
  publication tables.

- decimals:

  Decimal places that are reported.

## Value

|              |                                                            |
|--------------|------------------------------------------------------------|
|              |                                                            |
| `model`      | Character string from function call (see arguments above). |
| `contrast`   | List with contrast definitions.                            |
| `full.model` | Full regression model list.                                |
| `formula`    | Formula of the mplm model.                                 |

## Details

The function currently only supports single-case data (i.e., one case
per dataset). For multilevel piecewise regression models, please use the
[hplm](https://jazznbass.github.io/scan/reference/hplm.md) function.

## Functions

- `print(sc_mplm)`: Print results

- `export(sc_mplm)`: Export results as html

## See also

Other regression functions:
[`bplm()`](https://jazznbass.github.io/scan/reference/bplm.md),
[`fetch()`](https://jazznbass.github.io/scan/reference/fetch.md),
[`hplm()`](https://jazznbass.github.io/scan/reference/hplm.md),
[`plm()`](https://jazznbass.github.io/scan/reference/plm.md),
[`print.sc_ac()`](https://jazznbass.github.io/scan/reference/autocorr.md),
[`print.sc_bctau()`](https://jazznbass.github.io/scan/reference/corrected_tau.md),
[`trend()`](https://jazznbass.github.io/scan/reference/trend.md)

## Author

Juergen Wilbert

## Examples

``` r
res <- mplm(Leidig2018$`1a1`,
  dvar = c("academic_engagement", "disruptive_behavior")
)
print(res)
#> Multivariate piecewise linear model
#> 
#> Dummy model: W level = first, slope = first
#> Type III MANOVA 
#> Pillai = 0.35; F(6, 158) = 5.57; p = 0.000 
#> 
#>                        academic_engagement disruptive_behavior Pillai      F
#> Intercept                            2.771               0.849  0.284 15.475
#> Trend (mt)                          -0.216               0.082  0.032  1.275
#> Level phase B (phaseB)               2.340              -1.390  0.190  9.176
#> Slope phase B (interB)               0.219              -0.080  0.031  1.267
#>                            p
#> Intercept              0.000
#> Trend (mt)             0.285
#> Level phase B (phaseB) 0.000
#> Slope phase B (interB) 0.287
#> 
#> Formula: y ~ 1 + mt + phaseB + interB
## also report standardized coefficients:
print(res, std = TRUE)
#> Multivariate piecewise linear model
#> 
#> Dummy model: W level = first, slope = first
#> Type III MANOVA 
#> Pillai = 0.35; F(6, 158) = 5.57; p = 0.000 
#> 
#>                        academic_engagement disruptive_behavior Pillai      F
#> Intercept                            0.000               0.000  0.284 15.475
#> Trend (mt)                          -5.979               5.107  0.032  1.275
#> Level phase B (phaseB)               0.576              -0.767  0.190  9.176
#> Slope phase B (interB)               5.950              -4.897  0.031  1.267
#>                            p
#> Intercept              0.000
#> Trend (mt)             0.285
#> Level phase B (phaseB) 0.000
#> Slope phase B (interB) 0.287
#> 
#> Coefficients are standardized
#> Formula: y ~ 1 + mt + phaseB + interB
```

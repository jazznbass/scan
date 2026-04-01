# Between-Case Standardized Mean Difference

Calculates a standardized mean difference from a multilevel model as
described in Pustejovsky et al. (2014)

## Usage

``` r
between_smd(
  data,
  method = c("REML", "MCMCglmm"),
  ci = 0.95,
  include_residuals = TRUE,
  ...
)

# S3 method for class 'sc_bcsmd'
print(x, digits = 2, ...)

# S3 method for class 'sc_bcsmd'
export(
  object,
  caption = NA,
  footnote = NA,
  filename = NA,
  digits = 2,
  round = 2,
  ...
)
```

## Arguments

- data:

  Either an scdf or an object returned from the
  [`hplm()`](https://jazznbass.github.io/scan/reference/hplm.md) or
  [`bplm()`](https://jazznbass.github.io/scan/reference/bplm.md)
  function.

- method:

  Either `"REML"` or "`MCMglmm"`. This indicated which statistical
  method is applied to calculate the model.

- ci:

  A numeric between 0 and 1 setting the width of the confidence interval
  (when method is REML) or the credible interval (when method is
  MCMCglmm). The default is `0.95` for a 95-percent interval.

- include_residuals:

  Logical. See details.

- ...:

  Further arguments passed to the
  [`hplm()`](https://jazznbass.github.io/scan/reference/hplm.md) or
  [`bplm()`](https://jazznbass.github.io/scan/reference/bplm.md)function.

- x:

  An object returned by `baseline_smd()`.

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

## Value

An object of class sc_bcsmd. It is a list containing the following
elements:

- `models`: A list of data frames containing the BC-SMD results for each
  model calculated.

- `ci`: The width of the confidence/credible interval.

- `method`: The method used for model estimation.

## Details

The BC-SMD is calculate as
`BC-SMD = Phase difference / sqrt(residual + random_intercept)`. This is
most closely related to Cohen's *d*. If you want to have the most exact
estimation based on the between case variance, you have to exclude the
residual variance by setting the argument `include_residuals = FALSE`
you get `BC-SMD = Phase difference / sqrt(random_intercept)`. The 'base'
model only includes the phase level as a predictor like originally
proposed by Hedges et al. Whereas the 'Full plm' model includes the
trend and the phase slope as additional predictors.

## Functions

- `print(sc_bcsmd)`: Print results

- `export(sc_bcsmd)`: export results

## References

Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R. (2014).
Design-Comparable Effect Sizes in Multiple Baseline Designs: A General
Modeling Framework. Journal of Educational and Behavioral Statistics,
39(5), 368–393. https://doi.org/10.3102/1076998614547577

## Author

Juergen Wilbert

## Examples

``` r
## Create a example scdf:
des <- design(
  n = 150,
  phase_design = list(A1 = 10, B1 = 10, A2 = 10, B2 = 10, C = 10),
  level = list(B1 = 1, A2 = 0, B2 = 1, C = 1),
  rtt = 0.7,
  random_start_value = TRUE
)
study <- random_scdf(des)

## Standard BC-SMD return:
between_smd(study)
#> Between-Case Standardized Mean Difference
#> 
#> Method: REML
#> Base model
#> 
#>   Effect BC-SMD   se LL-CI95% UL-CI95%
#>  phaseB1   0.82 0.02     0.78     0.85
#>  phaseA2   0.82 0.02     0.78     0.86
#>  phaseB2   1.65 0.02     1.61     1.69
#>   phaseC   2.47 0.02     2.43     2.51
#> 
#> Full plm model
#> 
#>   Effect BC-SMD   se LL-CI95% UL-CI95%
#>  phaseB1   0.82 0.04     0.75     0.90
#>  phaseA2   0.80 0.08     0.64     0.95
#>  phaseB2   1.64 0.13     1.40     1.89
#>   phaseC   2.40 0.17     2.07     2.74

## Specify the model and provide an hplm object:
model <- hplm(study, contrast_level = "preceding", slope = FALSE,  trend = FALSE)
between_smd(model)
#> Between-Case Standardized Mean Difference
#> 
#> Method: REML
#> Provided
#> 
#>   Effect BC-SMD   se LL-CI95% UL-CI95%
#>  phaseB1   0.82 0.02     0.78     0.86
#>  phaseA2   0.00 0.02    -0.04     0.04
#>  phaseB2   0.83 0.02     0.79     0.87
#>   phaseC   0.82 0.02     0.78     0.86

## excluding the residuals gives a more accurate estimation:
between_smd(model, include_residuals = FALSE)
#> Between-Case Standardized Mean Difference
#> 
#> Method: REML
#> Provided
#> 
#>   Effect BC-SMD   se LL-CI95% UL-CI95%
#>  phaseB1   0.97 0.02     0.92     1.01
#>  phaseA2   0.00 0.02    -0.04     0.05
#>  phaseB2   0.98 0.02     0.94     1.03
#>   phaseC   0.97 0.02     0.93     1.02
```

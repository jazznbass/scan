# Baseline corrected tau

Kendall's tau correlation for the dependent variable and the phase
variable is calculated after correcting for a baseline trend.

## Usage

``` r
# S3 method for class 'sc_bctau'
print(x, nice = TRUE, digits = "auto", ...)

# S3 method for class 'sc_bctau'
export(
  object,
  caption = NA,
  footnote = NA,
  filename = NA,
  nice = TRUE,
  round = 2,
  ...
)

corrected_tau(
  data,
  dvar,
  pvar,
  mvar,
  phases = c(1, 2),
  alpha = 0.05,
  continuity = FALSE,
  repeated = FALSE,
  tau_method = c("b", "a")
)
```

## Arguments

- x:

  An object returned by `corrected_tau()`

- nice:

  If set TRUE (default) output values are rounded and optimized for
  publication tables.

- digits:

  The minimum number of significant digits to be use. If set to "auto"
  (default), values are predefined.

- ...:

  Further parameters passed to the print function

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

- phases:

  A vector of two characters or numbers indicating the two phases that
  should be compared. E.g., `phases = c("A","C")` or `phases = c(2,4)`
  for comparing the second to the fourth phase. Phases could be combined
  by providing a list with two elements. E.g.,
  `phases = list(A = c(1,3), B = c(2,4))` will compare phases 1 and 3
  (as A) against 2 and 4 (as B). Default is `phases = c(1,2)`.

- alpha:

  Sets the p-value at and below which a baseline correction is applied.

- continuity:

  If TRUE applies a continuity correction for calculating p

- repeated:

  If TRUE applies the repeated median method for calculating slope and
  intercept.

- tau_method:

  Character with values "a" or "b" (default) indicating whether Kendall
  Tau A or Kendall Tau B is applied.

## Details

This method has been proposed by Tarlow (2016). The baseline data are
checked for a significant autocorrelation (based on Kendall's Tau). If
so, a non-parametric Theil-Sen regression is applied for the baseline
data where the dependent values are regressed on the measurement time.
The resulting slope information is then used to predict data of the
B-phase. The dependent variable is now corrected for this baseline trend
and the residuals of the Theil-Sen regression are taken for further
calculations. Finally, Kendall's tau is calculated for the dependent
variable and the dichotomous phase variable. The function here provides
two extensions to this procedure: The more accurate Siegel repeated
median regression is applied when `repeated = TRUE` and a continuity
correction is applied when `continuity = TRUE`.

## Functions

- `print(sc_bctau)`: Print results

- `export(sc_bctau)`: Export results as html

## References

Tarlow, K. R. (2016). An Improved Rank Correlation Effect Size Statistic
for Single-Case Designs: Baseline Corrected Tau. *Behavior Modification,
41(4)*, 427–467. https://doi.org/10.1177/0145445516676750

## See also

Other regression functions:
[`bplm()`](https://jazznbass.github.io/scan/reference/bplm.md),
[`fetch()`](https://jazznbass.github.io/scan/reference/fetch.md),
[`hplm()`](https://jazznbass.github.io/scan/reference/hplm.md),
[`mplm()`](https://jazznbass.github.io/scan/reference/mplm.md),
[`plm()`](https://jazznbass.github.io/scan/reference/plm.md),
[`print.sc_ac()`](https://jazznbass.github.io/scan/reference/autocorr.md),
[`trend()`](https://jazznbass.github.io/scan/reference/trend.md)

## Examples

``` r
dat <- scdf(c(A = 33,25,17,25,14,13,15, B = 15,16,16,5,7,9,6,5,3,3,8,11,7))
corrected_tau(dat)
#> Baseline corrected tau
#> 
#> Method: Theil-Sen regression
#> Kendall's tau b applied.
#> Continuity correction not applied.
#> 
#> [case #1] :
#>                            tau     z     p
#> Baseline autocorrelation -0.68 -2.13  <.05
#> Uncorrected tau          -0.57 -2.94  <.01
#> Baseline corrected tau    0.70  3.61 <.001
#> 
#> Baseline correction should be applied.
#> 
#> 
```

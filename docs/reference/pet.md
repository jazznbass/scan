# Percent exceeding the trend (PET)

The `pet` function returns the percentage of Phase B data points that
exceed the prediction based on the Phase A trend. A binomial test
against a 50/50 distribution is calculated. It also calculates the
percentage of Phase B data points that exceed the upper (or lower) 95
percent confidence interval of the predicted progression.

## Usage

``` r
pet(data, dvar, pvar, mvar, ci = 0.95, decreasing = FALSE, phases = c(1, 2))
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

- ci:

  Width of the confidence interval. Default is `ci = 0.95`.

- decreasing:

  If you expect data to be lower in the B phase, set
  `decreasing = TRUE`. Default is `decreasing = FALSE`.

- phases:

  A vector of two characters or numbers indicating the two phases that
  should be compared. E.g., `phases = c("A","C")` or `phases = c(2,4)`
  for comparing the second to the fourth phase. Phases could be combined
  by providing a list with two elements. E.g.,
  `phases = list(A = c(1,3), B = c(2,4))` will compare phases 1 and 3
  (as A) against 2 and 4 (as B). Default is `phases = c(1,2)`.

## Value

|              |                                                            |
|--------------|------------------------------------------------------------|
|              |                                                            |
| `PET`        | Percent exceeding the trend.                               |
| `ci`         | Width of confidence interval.                              |
| `decreasing` | Logical argument from function call (see Arguments above). |

## Details

The PET is calculated by first fitting a linear model to the Phase A
data to estimate the trend. Then, for each data point in Phase B, it is
determined whether it exceeds the predicted value from the Phase A
trend. The PET is the percentage of Phase B data points that exceed this
predicted value. Additionally, a binomial test is performed to assess
whether the observed PET is significantly greater than what would be
expected by chance (i.e., 50%).

## See also

Other overlap functions:
[`ird()`](https://jazznbass.github.io/scan/reference/ird.md),
[`nap()`](https://jazznbass.github.io/scan/reference/nap.md),
[`overlap()`](https://jazznbass.github.io/scan/reference/overlap.md),
[`pand()`](https://jazznbass.github.io/scan/reference/pand.md),
[`pem()`](https://jazznbass.github.io/scan/reference/pem.md),
[`pnd()`](https://jazznbass.github.io/scan/reference/pnd.md),
[`print.sc_cdc()`](https://jazznbass.github.io/scan/reference/cdc.md),
[`select_phases()`](https://jazznbass.github.io/scan/reference/select_phases.md),
[`tau_u()`](https://jazznbass.github.io/scan/reference/tau_u.md)

## Author

Juergen Wilbert

## Examples

``` r

## Calculate the PET and use a 99%-CI for the additional calculation
# create random example data
design <- design(n = 5, slope = 0.2)
dat <- random_scdf(design, seed = 23)
pet(dat, ci = .99)
#> Percent Exceeding the Trend
#> 
#> 
#>       Case   PET PET CI  binom.p
#>  [case #1]  13.3    0.0 1.00e+00
#>  [case #2] 100.0   53.3 3.05e-05
#>  [case #3] 100.0   93.3 3.05e-05
#>  [case #4]   0.0    0.0 1.00e+00
#>  [case #5]  80.0    0.0 1.76e-02
#> 
#> Binom.test: alternative hypothesis: true probability > 50%
#> PET CI: Percent of values greater than upper 99% confidence threshold (single sided)
```

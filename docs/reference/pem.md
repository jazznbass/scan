# Percent exceeding the median (PEM)

The `pem` function returns the percentage of phase B data exceeding the
phase A median. Additionally, a chi square test against a 50/50
distribution is computed. Different measures of central tendency can be
addressed for alternative analyses.

## Usage

``` r
pem(
  data,
  dvar,
  pvar,
  decreasing = FALSE,
  binom.test = TRUE,
  chi.test = FALSE,
  FUN = median,
  phases = c(1, 2),
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

- decreasing:

  If you expect data to be lower in the B phase, set
  `decreasing = TRUE`. Default is `decreasing = FALSE`.

- binom.test:

  Computes a binomial test for a 50/50 distribution. Default is
  `binom.test = TRUE`.

- chi.test:

  Computes a Chi-square test. The default setting `chi.test = FALSE`
  skips the Chi-square test.

- FUN:

  Data points are compared with the phase A median. Use this argument to
  implement alternative measures of central tendency. Default is
  `FUN = median`.

- phases:

  A vector of two characters or numbers indicating the two phases that
  should be compared. E.g., `phases = c("A","C")` or `phases = c(2,4)`
  for comparing the second to the fourth phase. Phases could be combined
  by providing a list with two elements. E.g.,
  `phases = list(A = c(1,3), B = c(2,4))` will compare phases 1 and 3
  (as A) against 2 and 4 (as B). Default is `phases = c(1,2)`.

- ...:

  Additional arguments for the `FUN` parameter (e.g.
  `FUN = mean, trim = 0.1` will use the 10 percent trimmed arithmetic
  mean instead of the median for comparisons). The function must take a
  vector of numeric values and the `na.rm` argument and return a numeric
  value.

## Details

The Percent Exceeding the Median is calculated as the percentage of data
points in phase B that exceed the median of phase A. If the `decreasing`
argument is set to `TRUE`, the percentage of data points in phase B that
are below the median of phase A is calculated. The PEM is expressed as a
percentage ranging from 0 to 100. Higher values indicate a greater
degree of improvement from phase A to phase B.

## See also

Other overlap functions:
[`ird()`](https://jazznbass.github.io/scan/reference/ird.md),
[`nap()`](https://jazznbass.github.io/scan/reference/nap.md),
[`overlap()`](https://jazznbass.github.io/scan/reference/overlap.md),
[`pand()`](https://jazznbass.github.io/scan/reference/pand.md),
[`pet()`](https://jazznbass.github.io/scan/reference/pet.md),
[`pnd()`](https://jazznbass.github.io/scan/reference/pnd.md),
[`print.sc_cdc()`](https://jazznbass.github.io/scan/reference/cdc.md),
[`select_phases()`](https://jazznbass.github.io/scan/reference/select_phases.md),
[`tau_u()`](https://jazznbass.github.io/scan/reference/tau_u.md)

## Author

Juergen Wilbert

## Examples

``` r
## Calculate the PEM including the Binomial and Chi-square tests for a single-case
dat <- random_scdf(5, level = 0.5)
#> Warning: The first argument is expected to be a design matrix created by design(). If you want to set n, please name the first argument with n = ...
pem(dat, chi.test = TRUE)
#> Percent Exceeding the Median
#> 
#>       Case PEM positives total  binom.p Chi DF        p
#>  [case #1] 100        15    15 3.05e-05  15  1 0.000108
#> 
#> Alternative hypothesis: true probability > 50%
```

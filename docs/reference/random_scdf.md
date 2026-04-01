# Single-case data generator

The `random_scdf` function generates random single-case data frames for
monte-carlo studies and demonstration purposes. `design` is used to set
up a design matrix with all parameters needed for the `random_scdf`
function.

## Usage

``` r
random_scdf(design = NULL, round = NA, random_names = FALSE, seed = NULL, ...)
```

## Arguments

- design:

  A design matrix which is created by `design` and specifies all
  parameters. If `design` is `NULL` (default), the design parameters
  have to be specified via the `...` argument. If a numeric value is
  provided instead of a design matrix, it is interpreted as the number
  of cases `n`.

- round:

  Rounds the scores to the defined decimal. To round to the second
  decimal, set `round = 2`. Default is `NA` (no rounding).

- random_names:

  Is `FALSE` by default. If set `random_names = TRUE` cases are assigned
  random first names. If set `"neutral", "male" or "female"` only gender
  neutral, male, or female names are chosen. The names are drawn from
  the 2,000 most popular names for newborns in 2012 in the U.S. (1,000
  male and 1,000 female names).

- seed:

  A seed number for the random generator. If `NULL` (default), no seed

- ...:

  arguments that are directly passed to the `design` function for a more
  concise coding.

## Value

A single-case data frame. See
[`scdf`](https://jazznbass.github.io/scan/reference/scdf.md) to learn
about this format.

## Details

The generated data can be normally distributed, Poisson-distributed, or
binomially distributed. The default is normally distributed data.

## Author

Juergen Wibert

## Examples

``` r
## Create random single-case data and inspect it
design <- design(
  n = 3, rtt = 0.75, slope = 0.1, extreme_prop = 0.1,
  missing_prop = 0.1
)
dat <- random_scdf(design, round = 1, random_names = TRUE, seed = 123)
describe(dat)
#> Describe Single-Case Data
#> 
#>        Vanessa Bryn Tia
#> Design     A-B  A-B A-B
#> n.A          5    5   5
#> n.B         15   15  15
#> mis.A        0    1   0
#> mis.B        2    1   2
#> 
#>         Vanessa   Bryn    Tia
#> m.A       51.12  50.00  54.36
#> m.B      57.115 52.793 56.892
#> md.A       50.4   49.2   52.6
#> md.B      59.60  55.75  59.20
#> sd.A      4.672  3.631  4.538
#> sd.B     10.403 12.893  8.003
#> mad.A     2.520  2.076  3.410
#> mad.B     7.858 10.601  6.672
#> min.A      46.8   46.5   50.3
#> min.B      29.7   19.7   38.7
#> max.A      59.0   55.1   61.8
#> max.B      71.3   65.2   67.7
#> trend.A    0.95   1.36   2.27
#> trend.B   0.935  1.693  1.358

## And now have a look at poisson-distributed data
design <- design(
  n = 3, B_start = c(6, 10, 14), mt = c(12, 20, 22), start_value = 10,
  distribution = "poisson", level = -5, missing_prop = 0.1
)
dat <- random_scdf(design, seed = 1234)
pand(dat, decreasing = TRUE)
#> Percentage of all non-overlapping data
#> 
#> Method: sort 
#> 
#> PAND = 91.8%
#> Φ =  0.836  ; Φ² =  0.699 
#> 
#> 49 measurements (23 Phase A, 26 Phase B) in 3 cases
#> Overlapping data: n = 4 ; percentage = 8.2 
#> 
#> 2 x 2 Matrix of percentages
#>          A    B total
#> A     42.9  4.1  46.9
#> B      4.1 49.0  53.1
#> total 46.9 53.1 100.0
#> 
#> 2 x 2 Matrix of counts
#>        A  B total
#> A     21  2    23
#> B      2 24    26
#> total 23 26    49
#> 
#> 
#> Chi-Squared test:
#> X² = 34.256, df = 1, p = 0.000 
#> 
#> Fisher exact test:
#> Odds ratio = 99.881, p = 0.000 
```

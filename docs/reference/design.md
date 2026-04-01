# Generate a single-case design matrix for multiple random single-cases

Generates a parameter list used for generating multiple random
single-cases. This is used within the `random_scdf` function and the
`power_test` function and for other Monte-Carlo tasks.

## Usage

``` r
design(
  n = 1,
  phase_design = list(A = 5, B = 15),
  trend = 0,
  level = list(0),
  slope = list(0),
  start_value = 50,
  s = 10,
  rtt = 0.8,
  extreme_prop = list(0),
  extreme_range = c(-4, -3),
  missing_prop = 0,
  distribution = c("normal", "gaussian", "poisson", "binomial"),
  random_start_value = FALSE,
  n_trials = NULL,
  mt = NULL,
  B_start = NULL,
  m,
  phase.design,
  MT,
  B.start,
  extreme.p,
  extreme.d,
  missing.p
)
```

## Arguments

- n:

  Number of cases to be designed (Default is `n = 1`).

- phase_design, phase.design:

  A list defining the length and label of each phase. E.g.,
  `phase_design = list(A1 = 10, B1 = 10, A2 = 10, B2 = 10)`. Use vectors
  if you want to define different values for each case
  `phase_design = list(A = c(10, 15), B = c(10, 15)`.

- trend:

  Defines the effect size of a trend added incrementally to each
  measurement across the whole data-set. To assign different trends to
  several single-cases, use a vector of values (e.g.
  `trend = c(.1, .3, .5)`). If the number of cases exceeds the length of
  the vector, values are recycled. When using a 'gaussian' distribution,
  the `trend` parameters indicate effect size *d* changes. When using a
  binomial or poisson distribution, `trend` indicates an increase in
  points / counts per measurement.

- level:

  A list that defines the level increase (effect size *d*) at the
  beginning of each phase relative to the previous phase (e.g.
  `list(A = 0, B = 1)`). The first element must be zero as the first
  phase of a single-case has no level effect (if you have one less list
  element than the number of phases, scan will add a leading element
  with 0 values). Use vectors to define variable level effects for each
  case (e.g. `list(A = c(0, 0), B = c(1, 2))`). When using a 'gaussian'
  distribution, the `level` parameters indicate effect size *d* changes.
  When using a binomial or poisson distribution, `level` indicates an
  increase in points / counts with the onset of each phase.

- slope:

  A list that defines the increase per measurement for each phase
  compared to the previous phase. `slope = list(A = 0, B = .1)`
  generates an incremental increase of 0.1 per measurement starting at
  the B phase. The first list element must be zero as the first phase of
  a single-case has no slope effect (if you have one less list element
  than the number of phases, scan will add a leading element with 0
  values). Use vectors to define variable slope effects for each case
  (e.g. `list(A = c(0, 0), B = c(0.1, 0.2))`). If the number of cases
  exceeds the length of the vector, values are recycled. When using a
  'gaussian' distribution, the `slope` parameters indicate effect size
  *d* changes per measurement. When using a binomial or poisson
  distribution, `slope` indicates an increase in points / counts per
  measurement.

- start_value, m:

  Starting value at the first measurement. Default is `50`. When
  `distribution = "poission"` the start_value represents frequency. When
  `distribution = "binomial"` start_value must range between 0 and 1 and
  they represent the probability of on event. To assign different start
  values to several single-cases, use a vector of values (e.g.
  `c(50, 42, 56)`). If the number of cases exceeds the length of the
  vector, values are recycled. The `m` argument is deprecated.

- s:

  Standard deviation used to calculate absolute values from level,
  slope, trend effects and to calculate and error distribution from the
  `rtt` values. Set to `10` by default. To assign different variances to
  several single-cases, use a vector of values (e.g.
  `s = c(5, 10, 15)`). If the number of cases exceeds the length of the
  vector, values are recycled. if the distribution is 'poisson' or
  'binomial' s is not applied.

- rtt:

  Reliability of the underlying simulated measurements. Set `rtt = .8`
  by default. To assign different reliabilities to several single-cases,
  use a vector of values (e.g. `rtt = c(.6, .7, .8)`). If the number of
  cases exceeds the length of the vector, values are repeated. `rtt` has
  no effect when you're using binomial or poisson distributions.

- extreme_prop, extreme.p:

  Probability of extreme values. `extreme.p = .05` gives a five percent
  probability of an extreme value. A vector of values assigns different
  probabilities to multiple cases. If the number of cases exceeds the
  length of the vector, values are recycled.

- extreme_range, extreme.d:

  Range for extreme values. `extreme_range = c(-7,-6)` uses extreme
  values within a range of -7 and -6 . In case of a binomial or poisson
  distribution, `extreme_range` indicates frequencies. In case of a
  gaussian (or normal) distribution it indicates effect size d. Caution:
  the first value must be smaller than the second, otherwise the
  procedure will fail.

- missing_prop, missing.p:

  Portion of missing values. `missing_prop = 0.1` creates 10\\ different
  probabilities to multiple cases. If the number of cases exceeds the
  length of the vector, values are repeated.

- distribution:

  Distribution of the criteria varible. Default is `"normal"`. Possible
  values are `"normal"`, `"binomial"`, and `"poisson"`.

- random_start_value:

  If TRUE, the start_values are randomized based on the distribution.

- n_trials:

  If `distribution` (see below) is `"binomial"`, `n_trials` is the
  number of trials/observations/items. E.g., if you simulate accuracy
  data with 10 items per measurement, set `n_trials = 10`. To assign
  different n_trials to several single-cases, use a vector of values
  (e.g. `n_trials = c(10, 15, 20)`). If the number of cases exceeds the
  length of the vector, values are recycled.

- mt, MT:

  Number of measurements (in each study). Default is `mt = 20`.

- B_start, B.start:

  Phase B starting point. The default setting `B_start = 6` would assign
  the first five scores (of each case) to phase A, and all following
  scores to phase B. To assign different starting points for a set of
  multiple single-cases, use a vector of starting values (e.g.,
  `B_start = c(6, 7, 8)`). If the number of cases exceeds the length of
  the vector, values will be recycled.

## Value

An object of class sc_design.

## Author

Juergen Wibert

## Examples

``` r
 ## Create random single-case data and inspect it
 design <- design(
   n = 3, rtt = 0.75, slope = 0.1, extreme_prop = 0.1,
   missing_prop = 0.1
 )
 dat <- random_scdf(design, round = 1, random.names = TRUE, seed = 123)
 describe(dat)
#> Describe Single-Case Data
#> 
#>        [case #1] [case #2] [case #3]
#> Design       A-B       A-B       A-B
#> n.A            5         5         5
#> n.B           15        15        15
#> mis.A          0         1         0
#> mis.B          2         1         2
#> 
#>         [case #1] [case #2] [case #3]
#> m.A         51.12     50.00     54.36
#> m.B        57.115    52.793    56.892
#> md.A         50.4      49.2      52.6
#> md.B        59.60     55.75     59.20
#> sd.A        4.672     3.631     4.538
#> sd.B       10.403    12.893     8.003
#> mad.A       2.520     2.076     3.410
#> mad.B       7.858    10.601     6.672
#> min.A        46.8      46.5      50.3
#> min.B        29.7      19.7      38.7
#> max.A        59.0      55.1      61.8
#> max.B        71.3      65.2      67.7
#> trend.A      0.95      1.36      2.27
#> trend.B     0.935     1.693     1.358

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

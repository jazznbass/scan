# Empirical power analysis for single-case data

Conducts a Monte-Carlo study on the test-power and alpha-error
probability of a statistical function.

## Usage

``` r
power_test(
  design,
  method = c("plm_level", "rand", "tauU"),
  effect = "level",
  n_sim = 100,
  design_is_one_study = TRUE,
  alpha_test = TRUE,
  power_test = TRUE,
  binom_test = FALSE,
  binom_test_alpha = FALSE,
  binom_test_power = FALSE,
  binom_test_correct = FALSE,
  ci = FALSE,
  alpha_level = 0.05
)
```

## Arguments

- design:

  An object returned from the `design` function.

- method:

  A (named) list that defines the methods the power analysis is based
  on. Each element can contain a function (that takes an scdf file and
  returns a p value) or a character string (the name of predefined
  functions). default `method = list("plm_level", "rand", "tauU")`
  computes a power analysis based on
  [`tau_u()`](https://jazznbass.github.io/scan/reference/tau_u.md),
  [`rand_test()`](https://jazznbass.github.io/scan/reference/rand_test.md)
  and [`plm()`](https://jazznbass.github.io/scan/reference/plm.md)
  analyses. (Further predefined functions are: "plm_slope",
  "plm_poisson_level", "plm_poisson_slope", "hplm_level", "hplm_slope",
  "base_tau".

- effect:

  Either "level" or "slope". The respective effect of the provided
  design is set to 0 when computing the alpha-error proportion.

- n_sim:

  Number of sample studies created for the the Monte-Carlo study.
  Default is `n = 100`. Ignored if design_is_one_study = FALSE.

- design_is_one_study:

  If TRUE, the design is assumed to define all cases of one study that
  is repeatedly randomly created `n_sim` times. If FALSE, the design is
  assumed to contain all cases from which a random sample is generated.
  This is useful for very specific complex simulation studies. In this
  case, n_sim indicates how many random samples are drawn from the
  provided design. Default is TRUE.

- alpha_test:

  Logical. If TRUE, alpha error is calculated.

- power_test:

  Logical. If TRUE, power is calculated.

- binom_test:

  Shortcut. When set TRUE, binom_test_power is set to 0.80,
  binom_test_alpha is set to 0.05, and binom_test_correct is set to
  0.875.

- binom_test_alpha:

  Either FALSE or a value. If a value is provided, a binomial test is
  calculated testing if the alpha error proportion is less than the
  provided value.

- binom_test_power:

  Either FALSE or a value. If a value is provided, a binomial test is
  calculated testing if the power is greater than the provided value.

- binom_test_correct:

  Either FALSE or a value. If a value is provided, a binomial test is
  calculated testing if the correct proportion is greater than the
  provided value.

- ci:

  Either FALSE or a value. If a value is provided, confidence intervals
  at the provided level are calculated for power, alpha error, and
  correct proportions.

- alpha_level:

  Alpha level used to calculate the proportion of significant tests.
  Default is `alpha_level = 0.05`.

## Value

A data frame with the power, alpha error, and correct proportions for
each provided method. If binomial tests are requested, p values for
these tests are also provided. If confidence intervals are requested,
these are also provided.

## Details

Based on a
[`design()`](https://jazznbass.github.io/scan/reference/design.md)
object, a large number of single-cases are generated and re-analysed
with a provided statistical function. The proportion of significant
analyses is the test power. In a second step, a specified effect of the
design object is set to 0 and again single-cases are generated and
re-analysed. The proportion of significant analyses is the alpha error
probability.

## See also

[`random_scdf()`](https://jazznbass.github.io/scan/reference/random_scdf.md),
[`design()`](https://jazznbass.github.io/scan/reference/design.md)

## Author

Juergen Wilbert

## Examples

``` r
## Assume you want to conduct a single-case study with 15 measurements
## (phases: A = 6 and B = 9) using a highly reliable test and
## an expected level effect of d = 1.4.
## A (strong) trend effect is trend = 0.05. What is the power?
## (Note: n_sims is set to 10. Set n_sims to 1000 for a serious calculation.)
design <- design(
  n = 1, phase_design = list(A = 6, B = 9),
  rtt = 0.8, level = 1.4, trend = 0.05
)
power_test(design, n_sim = 10)
#> Test-Power in percent:
#> 
#>     Method Power Alpha Error Alpha:Beta Correct
#>  plm_level    70           0      1:Inf      85
#>       rand    70           0      1:Inf      85
#>       tauU   100          20      1:0.0      90

## Would you achieve higher power by setting up a MBD with three cases?
design <- design(
  n = 3, phase_design = list(A = 6, B = 9),
  rtt = 0.8, level = 1.4, trend = 0.05
)
power_test(design, n_sim=10, method=list("hplm_level", "rand", "tauU_meta"))
#> Test-Power in percent:
#> 
#>      Method Power Alpha Error Alpha:Beta Correct
#>  hplm_level   100           0      1:NaN     100
#>        rand   100           0      1:NaN     100
#>   tauU_meta   100          50      1:0.0      75
```

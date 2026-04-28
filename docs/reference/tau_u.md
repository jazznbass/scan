# Tau-U for single-case data

This function calculates indices of the Tau-U family as proposed by
Parker et al. (2011a). It allows to calculate Tau-U values for single
cases as well as overall Tau-U values across several single cases by
applying a meta analysis.

## Usage

``` r
tau_u(
  data,
  dvar,
  pvar,
  method = c("complete", "parker", "tarlow"),
  phases = c(1, 2),
  meta_analyses = TRUE,
  ci = 0.95,
  ci_method = c("z", "tau", "s"),
  meta_weight_method = c("z", "tau"),
  tau_method = c("b", "a"),
  continuity_correction = FALSE
)

# S3 method for class 'sc_tauu'
print(
  x,
  complete = FALSE,
  digits = "auto",
  select = c("Tau", "CI lower", "CI upper", "SD_S", "Z", "p"),
  nice_p = TRUE,
  ...
)

# S3 method for class 'sc_tauu'
export(
  object,
  caption = NA,
  footnote = NA,
  filename = NA,
  select = "auto",
  meta = FALSE,
  round = 3,
  decimals = 3,
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

- method:

  `"complete"` (default), `"parker"` or `"tarlow"`. The `"parker"`
  calculates the number of possible pairs as described in Parker et
  al. (2011) which might lead to tau-U values greater than 1. `"tarlow"`
  follows an online calculator and R code developed by Tarlow (2017).

- phases:

  A vector of two characters or numbers indicating the two phases that
  should be compared. E.g., `phases = c("A","C")` or `phases = c(2,4)`
  for comparing the second to the fourth phase. Phases could be combined
  by providing a list with two elements. E.g.,
  `phases = list(A = c(1,3), B = c(2,4))` will compare phases 1 and 3
  (as A) against 2 and 4 (as B). Default is `phases = c(1,2)`.

- meta_analyses:

  If TRUE, a meta analysis is conducted.

- ci:

  Confidence intervals level. If NULL or NA, no confidence intervals are
  calculated.

- ci_method:

  String to specify the method for calculating the standard error of
  tau. Either "tau", "z", or "s" (not recommended).

- meta_weight_method:

  String to specify the method for calculating the weights of the
  studies. Either "tau" or "z".

- tau_method:

  Character with values "a" or "b" (default) indicating whether Kendall
  Tau A or Kendall Tau B is applied. Ignored for methods 'tarlow' and
  'parker'.

- continuity_correction:

  If TRUE, a continuity correction is applied for calculating p-values
  of correlations (here: S will be reduced by one before calculating Z).
  Ignored for methods 'tarlow' and 'parker'.

- x:

  Object returned from `tau_u()`.

- complete:

  Print all parameters.

- digits:

  The minimum number of significant digits to be use. If set to "auto"
  (default), values are predefined.

- select:

  Character vector with name of variables to be included. When the
  vector is named, variables are renamed appropriately.

- nice_p:

  If TRUE, p-values are printed in publication friendly form.

- ...:

  Further arguments passed to the function.

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

- meta:

  If TRUE, the results of the meta analysis will be exported. If FALSE,
  each single-case is exported.

- round:

  Integer passed to the digits argument used to round values.

- decimals:

  Decimal places that are reported.

## Value

- table:

  A data frame containing statistics from the Tau-U family, including:
  Pairs, positive and negative comparisons, S, and Tau

- matrix:

  The matrix of comparisons used for calculating the statistics.

- tau_u:

  Tau-U value.

## Details

Tau-U is an inconsistently operationalized construct. Parker et al.
(2011b) describe a method which may result in Tau-U outside the \[-1;1\]
interval. A different implementation of the method (provided at
http://www.singlecaseresearch.org/calculators/tau-u) uses tau-b (instead
of tau-a as in the original formulation by Parker). Bossart et. al
(2018) describe inconsistencies in the results from this implementation
as well. Another problems lies in the calculation in overall Tau-U
values from several single cases. The function presented here applies a
meta-analysis to gain the overall values. Each tau value is weighted by
the inverse of the variance (ie. the tau standard error). The confidence
intervals for single cases are calculated by Fisher-Z transforming tau,
calculating the confidence intervals, and inverse transform them back to
tau (see Long & Cliff, 1997).

## Functions

- `print(sc_tauu)`: Print results

- `export(sc_tauu)`: Export results as html table

## References

Brossart, D. F., Laird, V. C., & Armstrong, T. W. (2018). Interpreting
Kendall’s Tau and Tau-U for single-case experimental designs. *Cogent
Psychology, 5(1)*, 1–26. https://doi.org/10.1080/23311908.2018.1518687.

Long, J. D., & Cliff, N. (1997). Confidence intervals for Kendall’s tau.
*British Journal of Mathematical and Statistical Psychology*, 50(1),
31–41. https://doi.org/10.1111/j.2044-8317.1997.tb01100.x

Parker, R. I., Vannest, K. J., & Davis, J. L. (2011a). Effect Size in
Single-Case Research: A Review of Nine Nonoverlap Techniques. *Behavior
Modification*, 35(4), 303–322. https://doi.org/10/dsdfs4

Parker, R. I., Vannest, K. J., Davis, J. L., & Sauber, S. B. (2011b).
Combining Nonoverlap and Trend for Single-Case Research: Tau-U.
*Behavior Therapy, 42*(2), 284–299.
https://doi.org/10.1016/j.beth.2010.08.006

Tarlow, K. R. (2017, March). Tau-U for single-case research (R code).
Retrieved from http://ktarlow.com/stats/

## See also

Other overlap functions:
[`ird()`](https://jazznbass.github.io/scan/reference/ird.md),
[`nap()`](https://jazznbass.github.io/scan/reference/nap.md),
[`overlap()`](https://jazznbass.github.io/scan/reference/overlap.md),
[`pand()`](https://jazznbass.github.io/scan/reference/pand.md),
[`pem()`](https://jazznbass.github.io/scan/reference/pem.md),
[`pet()`](https://jazznbass.github.io/scan/reference/pet.md),
[`pnd()`](https://jazznbass.github.io/scan/reference/pnd.md),
[`print.sc_cdc()`](https://jazznbass.github.io/scan/reference/cdc.md),
[`select_phases()`](https://jazznbass.github.io/scan/reference/select_phases.md)

## Author

Juergen Wilbert

## Examples

``` r

tau_u(Grosche2011$Eva)
#> Tau-U
#> Method: complete 
#> Applied Kendall's Tau-b
#> 95% CIs for tau are reported.
#> CI method: z
#> 
#> Case: Eva 
#>                              Tau CI lower CI upper  SD_S    Z     p
#> A vs. B                     0.38    -0.08     0.71 22.80 1.32   .19
#> A vs. B - Trend A           0.26    -0.22     0.64 23.42 1.41   .16
#> A vs. B + Trend B           0.49     0.05     0.77 28.08 2.85 <.001
#> A vs. B + Trend B - Trend A 0.49     0.04     0.77 28.58 2.90 <.001
#> 

## Replicate  tau-U calculation from Parker et al. (2011)
bob <- scdf(c(A = 2, 3, 5, 3, B = 4, 5, 5, 7, 6), name = "Bob")
res <- tau_u(bob, method = "parker")
print(res, complete = TRUE)
#> Tau-U
#> Method: parker 
#> Applied Kendall's Tau-a
#> 95% CIs for tau are reported.
#> CI method: z
#> 
#> Case: Bob 
#>                             pairs pos neg ties  S  D  Tau CI lower CI upper
#> A vs. B                        20  17   1    2 16 20 0.80     0.29     0.96
#> Trend A                         6   4   1    1  3  6 0.50    -0.89     0.99
#> Trend B                        10   8   1    1  7 10 0.70    -0.48     0.98
#> A vs. B - Trend A              20  18   5    3 13 20 0.65    -0.02     0.92
#> A vs. B + Trend B              30  25   2    3 23 30 0.77     0.21     0.95
#> A vs. B + Trend B - Trend A    36  26   6    4 20 36 0.56    -0.17     0.89
#>                             SD_S VAR_S SE_Tau    Z    p n
#> A vs. B                     8.16 66.67   0.40 2.00  .05 9
#> Trend A                     2.94  8.67   0.46 1.08  .28 4
#> Trend B                     4.08 16.67   0.40 1.77  .08 5
#> A vs. B - Trend A           8.48 71.86   0.42 1.53  .13 9
#> A vs. B + Trend B           8.91 79.37   0.30 2.58 <.05 9
#> A vs. B + Trend B - Trend A 9.35 87.33   0.26 2.14 <.05 9
#> 

## Request tau-U for all single-cases from the Grosche2011 data set
tau_u(Grosche2011)
#> Tau-U
#> Method: complete 
#> Applied Kendall's Tau-b
#> 95% CIs for tau are reported.
#> CI method: z
#> 
#> Tau-U meta analyses:
#> Weight method: z
#> 95% CIs are reported.
#> 
#>                        Model Tau_U   se CI lower CI upper    z    p
#>                      A vs. B 0.071 0.14   -0.193     0.33 0.52 0.60
#>            A vs. B - Trend A 0.083 0.14   -0.181     0.34 0.61 0.54
#>            A vs. B + Trend B 0.183 0.14   -0.082     0.42 1.36 0.17
#>  A vs. B + Trend B - Trend A 0.207 0.14   -0.057     0.44 1.54 0.12
#> 
#> Case: Eva 
#>                              Tau CI lower CI upper  SD_S    Z     p
#> A vs. B                     0.38    -0.08     0.71 22.80 1.32   .19
#> A vs. B - Trend A           0.26    -0.22     0.64 23.42 1.41   .16
#> A vs. B + Trend B           0.49     0.05     0.77 28.08 2.85 <.001
#> A vs. B + Trend B - Trend A 0.49     0.04     0.77 28.58 2.90 <.001
#> 
#> Case: Georg 
#>                               Tau CI lower CI upper  SD_S     Z   p
#> A vs. B                     -0.04    -0.44     0.37 31.49 -0.16 .87
#> A vs. B - Trend A            0.06    -0.36     0.45 32.18  0.34 .73
#> A vs. B + Trend B            0.14    -0.28     0.51 39.75  0.93 .35
#> A vs. B + Trend B - Trend A  0.19    -0.23     0.55 40.30  1.32 .19
#> 
#> Case: Olaf 
#>                               Tau CI lower CI upper  SD_S     Z   p
#> A vs. B                     -0.10    -0.52     0.35 25.92 -0.39 .70
#> A vs. B - Trend A           -0.06    -0.49     0.40 29.74 -0.34 .74
#> A vs. B + Trend B           -0.09    -0.51     0.37 27.15 -0.52 .61
#> A vs. B + Trend B - Trend A -0.07    -0.50     0.38 30.82 -0.45 .65
#> 
```

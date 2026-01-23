# Overlap indices for single-case data

The `overlap` function provides the most common overlap indices for
single-case data and some additional statistics.

## Usage

``` r
overlap(data, dvar, pvar, mvar, decreasing = FALSE, phases = c(1, 2))
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

|  |  |
|----|----|
|  |  |
| `overlap` | A data frame consisting of the following indices for each single-case for all cases: PND, PEM, PET, NAP, PAND, IRD, Tau-U (A vs. B - Trend A), Diff_mean, Diff_trend, SMD, Hedges-g. |
| `phases.A` | Selection for A phase. |
| `phases.B` | Selection for B phase. |
| `design` | Phase design. |

## Details

It computes PND, PEM, PET, NAP, PAND, IRD, Tau-U, mean difference,
difference in trend, SMD, and Hedges-g for each single-case included in
an scdf.

See corresponding functions of PND, PEM, PET, NAP, PAND for calculation.
Tau_U(A) reports "A vs. B - Trend A" whereas Tau_U(BA) reports "A vs.
B + Trend B - Trend A". Base_Tau is baseline corrected tau (correction
applied when autocorrelation in phase A is significant). Diff_mean is
the mean difference. Diff_trend is the difference in the regression
estimation of the dependent variable on measurement-time (`x ~ mt`) for
each phase. SMD is the mean difference divided by the standard deviation
of phase A. Hedges_g is the mean difference divided by the pooled
standard deviation: \\\sqrt{ (n_A - 1)sd_A^2 + (n_B - 1)sd_B^2 \over
n_A + n_B - 2 }\\ with a hedges correction applied: \\Hedges_g \* (1 -
\frac{3}{4n - 9} ) )\\.

## See also

[`pnd()`](https://jazznbass.github.io/scan/reference/pnd.md),
[`pem()`](https://jazznbass.github.io/scan/reference/pem.md),
[`pet()`](https://jazznbass.github.io/scan/reference/pet.md),
[`nap()`](https://jazznbass.github.io/scan/reference/nap.md),
[`pand()`](https://jazznbass.github.io/scan/reference/pand.md),
[`ird()`](https://jazznbass.github.io/scan/reference/ird.md),
[`tau_u()`](https://jazznbass.github.io/scan/reference/tau_u.md),
[`corrected_tau()`](https://jazznbass.github.io/scan/reference/corrected_tau.md)

Other overlap functions:
[`ird()`](https://jazznbass.github.io/scan/reference/ird.md),
[`nap()`](https://jazznbass.github.io/scan/reference/nap.md),
[`pand()`](https://jazznbass.github.io/scan/reference/pand.md),
[`pem()`](https://jazznbass.github.io/scan/reference/pem.md),
[`pet()`](https://jazznbass.github.io/scan/reference/pet.md),
[`pnd()`](https://jazznbass.github.io/scan/reference/pnd.md),
[`print.sc_cdc()`](https://jazznbass.github.io/scan/reference/cdc.md),
[`select_phases()`](https://jazznbass.github.io/scan/reference/select_phases.md),
[`tau_u()`](https://jazznbass.github.io/scan/reference/tau_u.md)

## Author

Juergen Wilbert

## Examples

``` r
## Display overlap indices for one single-case
overlap(Huitema2000, decreasing = TRUE)
#> Overlap Indices
#> 
#> Comparing phase 1 against phase 2 
#> 
#>              [case #1]
#> Design             A-B
#> PND                 40
#> PEM                 70
#> PET                100
#> NAP                 70
#> NAP rescaled        40
#> PAND                60
#> IRD                0.4
#> Tau_U(A)         -0.51
#> Tau_U(BA)        -0.69
#> Base_Tau         -0.73
#> Diff_mean        -20.4
#> Diff_trend      -17.87
#> SMD               -0.9
#> Hedges_g         -0.71
#> 
#> The following variables were used in this analysis:
#> 'score' as dependent variable, 'phase' as phase variable, and 'mt' as measurement-time variable.

## Display overlap indices for six single-cases
overlap(GruenkeWilbert2014)
#> Overlap Indices
#> 
#> Comparing phase 1 against phase 2 
#> 
#>              Anton   Bob  Paul Robert   Sam   Tim
#> Design         A-B   A-B   A-B    A-B   A-B   A-B
#> PND            100   100   100    100   100   100
#> PEM            100   100   100    100   100   100
#> PET            100   100   100    100   100   100
#> NAP            100   100   100    100   100   100
#> NAP rescaled   100   100   100    100   100   100
#> PAND           100   100   100    100   100   100
#> IRD              1     1     1      1     1     1
#> Tau_U(A)      0.67  0.66  0.75   0.68  0.65  0.68
#> Tau_U(BA)     0.48  0.57  0.60   0.49  0.51  0.35
#> Base_Tau      0.67  0.77  0.75   0.77  0.72  0.66
#> Diff_mean     4.14  5.82  5.00   4.78  4.48  6.00
#> Diff_trend    0.43  0.00  0.28  -0.08 -0.07  0.60
#> SMD           5.07  7.13  6.64   5.72  8.17  7.35
#> Hedges_g      5.06  6.50  6.53   4.90  5.36  6.11
#> 
#> The following variables were used in this analysis:
#> 'score' as dependent variable, 'phase' as phase variable, and 'mt' as measurement-time variable.

## Combining phases for analyszing designs with more than two phases
overlap(exampleA1B1A2B2, phases = list(c("A1","A2"), c("B1","B2")))
#> Overlap Indices
#> 
#> Comparing phases A1 + A2 against phases B1 + B2 
#> 
#>                    Pawel      Moritz      Jannis
#> Design       A1-B1-A2-B2 A1-B1-A2-B2 A1-B1-A2-B2
#> PND                   55          78          71
#> PEM                  100         100         100
#> PET                  100         100         100
#> NAP                   94          97          98
#> NAP rescaled          89          94          97
#> PAND                  85          85          90
#> IRD                 0.75        0.80        0.89
#> Tau_U(A)            0.54        0.44        0.43
#> Tau_U(BA)           0.45        0.46        0.38
#> Base_Tau            0.65        0.68        0.68
#> Diff_mean          12.25       13.58       15.27
#> Diff_trend         -0.05        0.00       -0.54
#> SMD                 2.68        3.27        3.62
#> Hedges_g            2.07        2.72        2.98
```

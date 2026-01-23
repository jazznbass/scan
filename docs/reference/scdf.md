# Single case data frame constructor

`scdf()` is the constructor for objects of class `scdf`. It stores data
from single-case studies in a structured format suitable for analysis
with the `scan` package.

## Usage

``` r
scdf(
  ...,
  B_start = NULL,
  phase_starts = NULL,
  phase_design = NULL,
  name = NULL,
  dvar = "values",
  pvar = "phase",
  mvar = "mt"
)
```

## Arguments

- ...:

  One or more vectors representing measurement variables. See the
  *Details* section.

- B_start:

  The first measurement point of phase B (simple coding; only applicable
  if the design follows a strict AB pattern).

- phase_starts:

  A named vector defining the label and measurement time of each phase
  start. For example:
  `phase_starts = c(A1 = 1, B1 = 6, A2 = 14, B2 = 19)`.

- phase_design:

  A named vector defining the length and label of each phase. For
  example: `phase_design = c(A1 = 10, B1 = 10, A2 = 10, B2 = 10)`.

- name:

  Optional name for the case.

- dvar:

  Character string with the name of the dependent variable. Defaults to
  the attributes in the scdf file.

- pvar:

  Character string with the name of the phase variable. Defaults to the
  attributes in the scdf file.

- mvar:

  Character string with the name of the measurement time variable.
  Defaults to the attributes in the scdf file.

## Value

Returns a single-case data frame `scdf` suitable for all functions in
the `scan` package.

## Details

If no variable matching the name of the dependent variable is provided
(the default name is `values`, which can be changed via the `dvar`
argument), and the first provided variable is unnamed, that variable
will be interpreted as the dependent variable.

If no measurement-time variable is provided (default name `mt`,
configurable via the `mvar` argument), measurement times are
automatically defined as a sequence `(1, 2, 3, ..., n)`.

If the dependent variable is a **named vector**, the names will be used
to define a phase design. For example,
`values = c(A = 2, 3, 5, 4, 3, B = 6, 5, 4, 3)` will be interpreted as
an AB phase design with five measurements in phase A and four in phase
B.

If a vector matching the name of the phase variable is provided, it will
be used to define the phase design directly. Otherwise, the phase design
can be defined in three alternative ways: via the `B_start` argument,
via the `phase_starts` argument, or via the `phase_design` argument.

If `B_start` is provided, a simple AB phase design is assumed, with
phase A starting at measurement time 1 and phase B starting at the
measurement time indicated by `B_start`. If `phase_starts` is provided,
the phase design is constructed based on the measurement times indicated
in the vector. If `phase_design` is provided, it is used directly to
define the phase design. If multiple of these options are provided, the
priority order is: `phase_design`, `phase_starts`, `B_start`, phase
variable in data frame, names of dependent variable.

If none of these options are provided, an error is raised.

The function can be used to create single-case data frames for multiple
cases separately, which can then be combined into a list for
multiple-case analyses.

See also the convenience function
[`transform.scdf`](https://jazznbass.github.io/scan/reference/transform.scdf.md)
to add new variables to an existing `scdf` object.

See the package vignettes for further examples on how to create and work
with `scdf` objects.

See the section on *Data structure* in the documentation of the
[`scan`](https://jazznbass.github.io/scan/reference/scan-package.md)
package for further details on the `scdf` data structure.

## See also

Other data manipulation functions:
[`add_l2()`](https://jazznbass.github.io/scan/reference/add_l2.md),
[`as.data.frame.scdf()`](https://jazznbass.github.io/scan/reference/as.data.frame.scdf.md),
[`as_scdf()`](https://jazznbass.github.io/scan/reference/as_scdf.md),
[`batch_apply()`](https://jazznbass.github.io/scan/reference/batch_apply.md),
[`fill_missing()`](https://jazznbass.github.io/scan/reference/fill_missing.md),
[`moving_median()`](https://jazznbass.github.io/scan/reference/transform.scdf.md),
[`print.sc_outlier()`](https://jazznbass.github.io/scan/reference/outlier.md),
[`ranks()`](https://jazznbass.github.io/scan/reference/ranks.md),
[`rescale()`](https://jazznbass.github.io/scan/reference/rescale.md),
[`select_cases()`](https://jazznbass.github.io/scan/reference/select_cases.md),
[`set_vars()`](https://jazznbass.github.io/scan/reference/set_vars.md),
[`shift()`](https://jazznbass.github.io/scan/reference/shift.md),
[`smooth_cases()`](https://jazznbass.github.io/scan/reference/smooth_cases.md),
[`standardize()`](https://jazznbass.github.io/scan/reference/standardize.md),
[`truncate_phase()`](https://jazznbass.github.io/scan/reference/truncate_phase.md)

## Author

Juergen Wilbert

## Examples

``` r
## Scores on a letter naming task were collected on eleven days in a row.
## The intervention started after the fifth measurement,
## so the first B phase measurement was 6 (B_start = 6).
klaas <- scdf(
  c(5, 7, 8, 5, 7, 12, 16, 18, 15, 14, 19),
  B_start = 6, name = "Klaas"
)
describe(klaas)
#> Describe Single-Case Data
#> 
#>        Klaas
#> Design   A-B
#> n.A        5
#> n.B        6
#> mis.A      0
#> mis.B      0
#> 
#>          Klaas
#> m.A        6.4
#> m.B     15.667
#> md.A         7
#> md.B      15.5
#> sd.A     1.342
#> sd.B     2.582
#> mad.A    1.483
#> mad.B    2.965
#> min.A        5
#> min.B       12
#> max.A        8
#> max.B       19
#> trend.A    0.2
#> trend.B  0.743

# Alternative: using named vector
klaas <- scdf(
  c(A = 5, 7, 8, 5, 7, B = 12, 16, 18, 15, 14, 19),
  name = "Klaas"
)

# Alternative: using phase_design
klaas <- scdf(
  c(5, 7, 8, 5, 7, 12, 16, 18, 15, 14, 19),
  phase_design = c(A = 5, B = 6), name = "Klaas"
)

# Alternative: using phase_starts
klaas <- scdf(
  c(5, 7, 8, 5, 7, 12, 16, 18, 15, 14, 19),
  phase_starts = c(A = 1, B = 7), name = "Klaas"
)

## Unfortunately in a similar study there were no data collected on
## days 3 and 9. Use NA to pass them to the function:
emmi <- scdf(c(5, 7, NA, 5, 7, 12, 16, 18, NA, 14, 19),
  phase_design = c(A = 5, B = 6), name = "Emmi"
)
describe(emmi)
#> Describe Single-Case Data
#> 
#>        Emmi
#> Design  A-B
#> n.A       5
#> n.B       6
#> mis.A     1
#> mis.B     1
#> 
#>          Emmi
#> m.A         6
#> m.B      15.8
#> md.A        6
#> md.B       16
#> sd.A    1.155
#> sd.B    2.864
#> mad.A   1.483
#> mad.B   2.965
#> min.A       5
#> min.B      12
#> max.A       7
#> max.B      19
#> trend.A   0.2
#> trend.B 0.779

## In a MBD over three cases, data were collected eleven days in a row.
## Intervention starting points differ between subjects as they were
## randomly assigned. The three SCDFs are then combined in a list for
## further conjoined analyses.
charlotte <- scdf(c(A = 5, 7, 10, 5, 12, B = 7, 10, 18, 15, 14, 19))
theresa <- scdf(c(A = 3, 4, 3, 5, B = 7, 4, 7, 9, 8, 10, 12))
tonia <- scdf(c(A = 9, 8, 8, 7, 5, 7, B = 6, 14, 15, 12, 16))
mbd <- c(charlotte, theresa, tonia)
names(mbd) <- c("Charlotte", "Theresa", "Tonia")
overlap(mbd)
#> Overlap Indices
#> 
#> Comparing phase 1 against phase 2 
#> 
#>              Charlotte Theresa Tonia
#> Design             A-B     A-B   A-B
#> PND                 67      86    80
#> PEM                 83     100    80
#> PET                 33      86   100
#> NAP                 87      95    83
#> NAP rescaled        73      89    67
#> PAND                82      82    82
#> IRD               0.63    0.80  0.82
#> Tau_U(A)          0.37    0.52  0.63
#> Tau_U(BA)         0.49    0.71  0.69
#> Base_Tau          0.56    0.66  0.66
#> Diff_mean         6.03    4.39  5.27
#> Diff_trend        0.77    0.50  2.37
#> SMD               1.94    4.59  3.85
#> Hedges_g          1.37    1.87  1.70

## In a classroom-based intervention it was not possible to measure outcomes
## every day, but only on schooldays. The sequence of measurements is passed
## to the package by using a vector of measurement times.
frida <- scdf(
  c(A = 3, 2, 4, 2, 2, 3, 5, 6, B = 8, 10, 8, 12, 14, 13, 12),
  mt = c(1, 2, 3, 4, 5, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18)
)
summary(frida)
#> #A single-case data frame with one case
#> 
#>            Measurements Design
#>  [case #1]           15    A-B
#> 
#> Variable names:
#> values <dependent variable>
#> phase <phase variable>
#> mt <measurement-time variable>
#> 
describe(frida)
#> Describe Single-Case Data
#> 
#>        [case #1]
#> Design       A-B
#> n.A            8
#> n.B            7
#> mis.A          0
#> mis.B          0
#> 
#>         [case #1]
#> m.A         3.375
#> m.B            11
#> md.A            3
#> md.B           12
#> sd.A        1.506
#> sd.B         2.38
#> mad.A       1.483
#> mad.B       2.965
#> min.A           2
#> min.B           8
#> max.A           6
#> max.B          14
#> trend.A     0.305
#> trend.B      0.71

## example with two independent variables and four phases
jim <- scdf(
  zvt = c(47, 58, 76, 63, 71, 59, 64, 69, 72, 77, 76, 73),
  d2 = c(131, 134, 141, 141, 140, 140, 138, 140, 141, 140, 138, 140),
  phase_design = c(A1 = 3, B1 = 3, A2 = 3, B2 = 3), dvar = "zvt"
)
overlap(jim, phases = list(c("A1", "A2"), c("B1", "B2")))
#> Overlap Indices
#> 
#> Comparing phases A1 + A2 against phases B1 + B2 
#> 
#>                [case #1]
#> Design       A1-B1-A2-B2
#> PND                   17
#> PEM                   67
#> PET                   67
#> NAP                   68
#> NAP rescaled          36
#> PAND                  67
#> IRD                 0.33
#> Tau_U(A)            0.07
#> Tau_U(BA)           0.14
#> Base_Tau            0.27
#> Diff_mean            5.5
#> Diff_trend         -0.31
#> SMD                 0.52
#> Hedges_g            0.56
#> 
#> The following variables were used in this analysis:
#> 'zvt' as dependent variable, 'phase' as phase variable, and 'mt' as measurement-time variable.
```

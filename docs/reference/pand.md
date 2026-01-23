# Percentage of all non-overlapping data

The `pand()` function calculates the percentage of all non-overlapping
data (PAND; Parker, Hagan-Burke, & Vannest, 2007), an index to quantify
a level increase (or decrease) in performance after the onset of an
intervention.

## Usage

``` r
pand(
  data,
  dvar,
  pvar,
  decreasing = FALSE,
  phases = c(1, 2),
  method = c("sort", "minimum")
)

# S3 method for class 'sc_pand'
print(x, ...)

# S3 method for class 'sc_pand'
export(object, caption = NA, footnote = NA, filename = NA, round = 1, ...)
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

- phases:

  A vector of two characters or numbers indicating the two phases that
  should be compared. E.g., `phases = c("A","C")` or `phases = c(2,4)`
  for comparing the second to the fourth phase. Phases could be combined
  by providing a list with two elements. E.g.,
  `phases = list(A = c(1,3), B = c(2,4))` will compare phases 1 and 3
  (as A) against 2 and 4 (as B). Default is `phases = c(1,2)`.

- method:

  Either `"sort"`" or `"minimum"`. See details.

- x:

  An object returned by `pand()`

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

- round:

  Integer passed to the digits argument used to round values.

## Value

|  |  |
|----|----|
|  |  |
| `pand` | Percentage of all non-overlapping data. |
| `method` | Calculation method. |
| `phi` | Effect size Phi based on expected and observed values. |
| `perc_overlap` | Percentage of overlapping data points. |
| `overlaps` | Number of overlapping data points. |
| `n` | Number of data points. |
| `N` | Number of cases. |
| `n_a` | Number of data points in phase A. |
| `n_b` | Number of data points in phase B. |
| `matrix` | 2x2 frequency matrix of phase A and B comparisons. |
| `matrix_counts` | 2x2 counts matrix of phase A and B comparisons. |
| `chi_test` | A Chi-squared analysis of expected and observed data (chisq.test()). |
| `fisher_test` | A Fisher exact test analysis of expected and observed data (fisher.test()). |

## Details

PAND was proposed by Parker, Hagan-Burke, and Vannest in 2007. The
authors emphasize that PAND is designed for application in a multiple
case design with a substantial number of measurements, technically at
least 20 to 25, but preferably 60 or more. PAND is defined as 100% minus
the percentage of data points that need to be removed from either phase
in order to ensure nonoverlap between the phases. Several approaches
have been suggested to calculate PAND, leading to potentially different
outcomes. In their 2007 paper, Parker and colleagues present an
algorithm for computing PAND. The algorithm involves sorting the scores
of a time series, including the associated phases, and comparing the
resulting phase order with the original phase order using a contingency
table. To account for ties, the algorithm includes a randomization
process where ties are randomly assigned to one of the two phases.
Consequently, executing the algorithm multiple times could yield
different results. It is important to note that this algorithm does not
produce the same results as the PAND definition provided earlier in the
same paper. However, it offers the advantage of allowing the calculation
of an effect size measure `phi`, and the application of statistical
tests for frequency distributions. Pustejovsky (2019) presented a
mathematical formulation of Parker's original definition for comparing
two phases of a single case: \$\$PAND =
\frac{1}{m+n}max\\(i+j)I(y^A\_{i}\<y^B\_{n+1-j}\\\$\$ This formulation
provides accurate results for PAND, but the original definition has the
drawback of an unknown distribution under the null hypothesis, making a
statistical test difficult. The `pand()` function enables the
calculation of PAND using both methods. The first approach
(`method = "sort"`) follows the algorithm described above, with the
exclusion of randomization before sorting to avoid ambiguity. It
calculates a phi measure and provides the results of a chi-squared test
and a Fisher exact test. The second approach (`method = "minimum"`)
applies the aforementioned formula. The code of this function is based
on the code of the `SingleCaseES` package (function `calc_PAND`). For a
multiple case design, overlaps are calculated for each case, summed, and
then divided by the total number of measurements. No statistical test is
conducted for this method.

## Functions

- `print(sc_pand)`: Print results

- `export(sc_pand)`: Export results as html table (see
  [`export()`](https://jazznbass.github.io/scan/reference/export.md))

## References

Parker, R. I., Hagan-Burke, S., & Vannest, K. (2007). Percentage of All
Non-Overlapping Data (PAND): An Alternative to PND. *The Journal of
Special Education, 40*, 194-204.

Parker, R. I., & Vannest, K. (2009). An Improved Effect Size for
Single-Case Research: Nonoverlap of All Pairs. *Behavior Therapy, 40*,
357-367.

Pustejovsky, J. E. (2019). Procedural sensitivities of effect sizes for
single-case designs with directly observed behavioral outcome measures.
*Psychological Methods*, *24(2)*, 217-235.
https://doi.org/10.1037/met0000179

Pustejovsky JE, Chen M, Swan DM (2023). SingleCaseES: A Calculator for
Single-Case Effect Sizes. R package version 0.7.1.9999,
https://jepusto.github.io/SingleCaseES/.

## See also

Other overlap functions:
[`ird()`](https://jazznbass.github.io/scan/reference/ird.md),
[`nap()`](https://jazznbass.github.io/scan/reference/nap.md),
[`overlap()`](https://jazznbass.github.io/scan/reference/overlap.md),
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
## REplication of the Parker et al. 2007 example
pand(Parker2007)
#> Percentage of all non-overlapping data
#> 
#> Method: sort 
#> 
#> PAND = 85.7%
#> Φ =  0.713  ; Φ² =  0.508 
#> 
#> 28 measurements (13 Phase A, 15 Phase B) in 3 cases
#> Overlapping data: n = 4 ; percentage = 14.3 
#> 
#> 2 x 2 Matrix of percentages
#>          A    B total
#> A     39.3  7.1  46.4
#> B      7.1 46.4  53.6
#> total 46.4 53.6 100.0
#> 
#> 2 x 2 Matrix of counts
#>        A  B total
#> A     11  2    13
#> B      2 13    15
#> total 13 15    28
#> 
#> 
#> Chi-Squared test:
#> X² = 14.227, df = 1, p = 0.000 
#> 
#> Fisher exact test:
#> Odds ratio = 29.007, p = 0.000 

## Calculate the PAND with an expected decrease of phase B scores
cubs <- scdf(c(20,22,24,17,21,13,10,9,20,9,18), B_start = 5)
pand(cubs, decreasing = TRUE)
#> Percentage of all non-overlapping data
#> 
#> Method: sort 
#> 
#> PAND = 63.6%
#> Φ =  0.214  ; Φ² =  0.046 
#> 
#> 11 measurements (4 Phase A, 7 Phase B) in 1 cases
#> Overlapping data: n = 4 ; percentage = 36.4 
#> 
#> 2 x 2 Matrix of percentages
#>          A    B total
#> A     18.2 18.2  36.4
#> B     18.2 45.5  63.6
#> total 36.4 63.6 100.0
#> 
#> 2 x 2 Matrix of counts
#>       A B total
#> A     2 2     4
#> B     2 5     7
#> total 4 7    11
#> 
#> 
#> Chi-Squared test:
#> X² = 0.505, df = 1, p = 0.477 
#> 
#> Fisher exact test:
#> Odds ratio = 2.288, p = 0.576 
```

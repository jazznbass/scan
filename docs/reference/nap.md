# Nonoverlap of all Pairs (NAP)

The `nap()` function calculates the nonoverlap of all pairs (NAP; Parker
& Vannest, 2009). NAP summarizes the overlap between all pairs of phase
A and phase B data points. If an increase of phase B scores is expected,
a non-overlapping pair has a higher phase B data point. The NAP equals
*number of pairs showing no overlap / number of pairs* where ties are
counted as half non-overlaps. Because NAP can take values between 0 and
100 percent where values below 50 percent indicate an inverse effect, an
nap rescaled from -100 to 100 percent where negative values indicate an
inverse effect is also displayed (\\nap\_{rescaled} = 2 \* nap - 100\\).

## Usage

``` r
nap(data, dvar, pvar, decreasing = FALSE, phases = c(1, 2))
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

## Value

|       |                                                            |
|-------|------------------------------------------------------------|
|       |                                                            |
| `nap` | A data frame with NAP and additional values for each case. |
| `N`   | Number of cases.                                           |

## Details

If a decrease of phase B scores is expected, set the argument
`decreasing = TRUE`.

## References

Parker, R. I., & Vannest, K. (2009). An improved effect size for
single-case research: Nonoverlap of all pairs. *Behavior Therapy*, *40*,
357-367.

## See also

Other overlap functions:
[`ird()`](https://jazznbass.github.io/scan/reference/ird.md),
[`overlap()`](https://jazznbass.github.io/scan/reference/overlap.md),
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

## Calculate NAP for a study with  lower expected phase B scores
## (e.g. aggressive behavior)
gretchen <- scdf(c(A = 12, 14, 9, 10, B = 10, 6, 4, 5, 3, 4))
nap(gretchen, decreasing = TRUE)
#> Nonoverlap of All Pairs
#> 
#>       Case NAP NAP Rescaled  w    p   d   R²
#>  [case #1]  94           88 22 <.05 2.2 0.56

## Request NAP for all cases from the Grosche2011 scdf
nap(Grosche2011)
#> Nonoverlap of All Pairs
#> 
#>   Case NAP NAP Rescaled  w   p      d     R²
#>    Eva  69         38.5 24 .10  0.747 0.1223
#>  Georg  48         -4.2 62 .57 -0.072 0.0013
#>   Olaf  45        -10.4 53 .66 -0.176 0.0077

## Calculate NAP for phase 1 and phase 3 of an ABAB design
nap(exampleABAB, phases = c(1, 3))
#> Nonoverlap of All Pairs
#> 
#>     Case NAP NAP Rescaled  w   p     d     R²
#>   Howard  44        -13.0 56 .70 -0.22 0.0118
#>  Sheldon  37        -26.7 66 .84 -0.43 0.0451
#>  Leonard  47         -6.7 64 .60 -0.11 0.0032

```

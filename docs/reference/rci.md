# Reliable change index

The `rci()` function computes indices of reliable change (Wise, 2004)
and corresponding descriptive statistics.

## Usage

``` r
rci(data, dvar, pvar, rel, ci = 0.95, graph = FALSE, phases = c(1, 2))
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

- rel:

  Reliability of the measure, used to compute the standard error.

- ci:

  Width of confidence interval as a decimal. Default is `ci = 0.95`
  applying a 95 percent confidence interval.

- graph:

  If set `TRUE`, a box plot of phase A and B scores is displayed.
  `graph = FALSE` by default.

- phases:

  A vector of two characters or numbers indicating the two phases that
  should be compared. E.g., `phases = c("A","C")` or `phases = c(2,4)`
  for comparing the second to the fourth phase. Phases could be combined
  by providing a list with two elements. E.g.,
  `phases = list(A = c(1,3), B = c(2,4))` will compare phases 1 and 3
  (as A) against 2 and 4 (as B). Default is `phases = c(1,2)`.

## Details

The reliable change index (RCI) indicates whether the change in scores
from phase A to phase B is statistically reliable, that is, whether the
change is larger than would be expected due to measurement error alone.
The RCI can be calculated using different methods (Jacobsen & Truax,
1991; Christensen & Mendoza, 1986). The most common method is that of
Jacobsen and Truax (1991) which uses the standard error of measurement
in phase A to calculate the RCI. Christensen and Mendoza (1986) proposed
an alteration of the RCI which uses the standard error of the difference
between phase A and phase B means. Both methods are implemented in the
`rci()` function.

## References

Christensen, L., & Mendoza, J. L. (1986). A method of assessing change
in a single subject: An alteration of the RC index. *Behavior Therapy,
17*, 305-308.

Jacobson, N. S., & Truax, P. (1991). Clinical Significance: A
statistical approach to defining meaningful change in psychotherapy
research. *Journal of Consulting and Clinical Psychology, 59*, 12-19.

Wise, E. A. (2004). Methods for analyzing psychotherapy outcomes: A
review of clinical significance, reliable change, and recommendations
for future directions. *Journal of Personality Assessment, 82*, 50 - 59.

## Author

Juergen Wilbert

## Examples

``` r

## Report the RCIs of the first case from the byHeart data and include a graph
rci(byHeart2011[1], graph = TRUE, rel = 0.8)

#> Reliable Change Index
#> 
#> Mean Difference =  12.4 
#> Standardized Difference =  1.691 
#> Standard error of differences =  0.346 
#> Reliability of measurements =  0.8 
#> 
#> Descriptives:
#>         n mean    SD    SE
#> A-Phase 5  0.4 0.548 0.245
#> B-Phase 5 12.8 5.634 2.520
#> 
#> 95 % Confidence Intervals:
#>          Lower  Upper
#> A-Phase -0.080  0.880
#> B-Phase  7.862 17.738
#> 
#> Reliable Change Indices:
#>                            RCI
#> Jacobson et al.         50.623
#> Christensen and Mendoza 35.796
#> 
```

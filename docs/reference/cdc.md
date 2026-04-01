# Conservative Dual-Criterion Method

The `cdc()` function applies the Conservative Dual-Criterion Method
(Fisher, Kelley, & Lomas, 2003) to scdf objects. It compares phase B
data points to both phase A mean and trend (OLS, bi-split, tri-split)
with an additional increase/decrease of .25 SD. A binomial test against
a 50/50 distribution is computed and p-values below .05 are labelled
"systematic change".

## Usage

``` r
# S3 method for class 'sc_cdc'
print(x, nice = TRUE, ...)

# S3 method for class 'sc_cdc'
export(object, caption = NA, footnote = NA, filename = NA, nice = TRUE, ...)

cdc(
  data,
  dvar,
  pvar,
  mvar,
  decreasing = FALSE,
  trend_method = c("OLS", "bisplit", "trisplit"),
  conservative = 0.25,
  phases = c(1, 2)
)
```

## Arguments

- x:

  Object

- nice:

  If set TRUE (default) output values are rounded and optimized for
  publication tables.

- ...:

  Further parameters passed to the print function

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

- trend_method:

  Method used to calculate the trend line. Default is
  `trend_method = "OLS"`. Possible values are: `"OLS"`, `"bisplit"`, and
  `"trisplit"`. `"bisplit"`, and `"trisplit"` should only be used for
  cases with at least five data-points in both relevant phases.

- conservative:

  The CDC method adjusts the original mean and trend lines by adding
  (expected increase) or subtracting (expected decrease) an additional
  .25 SD before evaluating phase B data. Default is the CDC method with
  `conservative = .25`. To apply the Dual-Criterion (DC) method, set
  `conservative = 0`.

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
| `cdc` | CDC Evaluation based on a p-value below .05. |
| `cdc_exc` | Number of phase B datapoints indicating expected change. |
| `cdc_nb` | Number of phase B datapoints. |
| `cdc_p` | P value of Binomial Test. |
| `cdc_all` | Overall CDC Evaluation based on all instances/cases of a Multiple Baseline Design. |
| `N` | Number of cases. |
| `decreasing` | Logical argument from function call (see Arguments above). |
| `conservative` | Numeric argument from function call (see Arguments above). |
| `case_names` | Assigned name of single-case. |
| `phases` | \- |

## Functions

- `print(sc_cdc)`: Print results

- `export(sc_cdc)`: Export html results

## References

Fisher, W. W., Kelley, M. E., & Lomas, J. E. (2003). Visual Aids and
Structured Criteria for Improving Visual Inspection and Interpretation
of Single-Case Designs. *Journal of Applied Behavior Analysis, 36*,
387-406. https://doi.org/10.1901/jaba.2003.36-387

## See also

Other overlap functions:
[`ird()`](https://jazznbass.github.io/scan/reference/ird.md),
[`nap()`](https://jazznbass.github.io/scan/reference/nap.md),
[`overlap()`](https://jazznbass.github.io/scan/reference/overlap.md),
[`pand()`](https://jazznbass.github.io/scan/reference/pand.md),
[`pem()`](https://jazznbass.github.io/scan/reference/pem.md),
[`pet()`](https://jazznbass.github.io/scan/reference/pet.md),
[`pnd()`](https://jazznbass.github.io/scan/reference/pnd.md),
[`select_phases()`](https://jazznbass.github.io/scan/reference/select_phases.md),
[`tau_u()`](https://jazznbass.github.io/scan/reference/tau_u.md)

## Author

Timo Lueke

## Examples

``` r
## Apply the CDC method (standard OLS line)
design <- design(n = 1, slope = 0.2)
dat <- random_scdf(design, seed = 42)
cdc(dat)
#> Conservative Dual Criterion
#> 
#> N cases =  1 
#> 
#>       Case nB improve nB binom p    CDC Evaluation
#>  [case #1]         14 15   <.001 systematic change
#> 
#> Assuming an expected increase in phase B.
#> Alternative hypothesis (Binomial test): true probability > 50%

## Apply the CDC with Koenig's bi-split and an expected decrease in phase B.
cdc(exampleAB_decreasing, decreasing = TRUE, trend_method = "bisplit")
#> Conservative Dual Criterion
#> 
#> N cases =  3 
#> 
#>   Case nB improve nB binom p    CDC Evaluation
#>  Peter          7 13     .50         no change
#>   Tony         11 12    <.01 systematic change
#>  Bruce         14 14   <.001 systematic change
#> 
#> Assuming an expected decrease in phase B.
#> Alternative hypothesis (Binomial test): true probability < 50%
#> Overall evaluation of all MBD instances:   no change 

## Apply the CDC with Tukey's tri-split, comparing the first and fourth phase
cdc(exampleABAB, trend_method = "trisplit", phases = c(1,4))
#> Conservative Dual Criterion
#> 
#> N cases =  3 
#> 
#>     Case nB improve nB binom p    CDC Evaluation
#>   Howard         10 10   <.001 systematic change
#>  Sheldon          4 10     .82         no change
#>  Leonard          7  7    <.01 systematic change
#> 
#> Assuming an expected increase in phase B.
#> Alternative hypothesis (Binomial test): true probability > 50%
#> Overall evaluation of all MBD instances:   no change 

## Apply the Dual-Criterion (DC) method (i.e., mean and trend without
##shifting).
cdc(
 exampleAB_decreasing,
 decreasing = TRUE,
 trend_method = "bisplit",
 conservative = 0
)
#> Conservative Dual Criterion
#> 
#> N cases =  3 
#> 
#>   Case nB improve nB binom p    CDC Evaluation
#>  Peter          7 13     .50         no change
#>   Tony         12 12   <.001 systematic change
#>  Bruce         14 14   <.001 systematic change
#> 
#> Assuming an expected decrease in phase B.
#> Alternative hypothesis (Binomial test): true probability < 50%
#> Overall evaluation of all MBD instances:   no change 

```

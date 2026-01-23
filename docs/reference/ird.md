# IRD - Improvement rate difference

`ird()` calculates the robust improvement rate difference as proposed by
Parker and colleagues (2011).

## Usage

``` r
ird(data, dvar, pvar, decreasing = FALSE, phases = c(1, 2))

# S3 method for class 'sc_ird'
print(x, digits = 3, ...)

# S3 method for class 'sc_ird'
export(object, caption = NA, footnote = NA, filename = NA, round = 3, ...)
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

- x:

  An object returned by `ird()`

- digits:

  The minimum number of significant digits to be use.

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

## Details

The adaptation of the improvement rate difference for single-case phase
comparisons was developed by Parker and colleagues (2009). A variation
called robust improvement rate difference was proposed by Parker and
colleagues in 2011. This function calculates the robust improvement rate
difference. It follows the formula suggested by Pustejovsky (2019). For
a multiple case design, ird is based on the overall improvement rate of
all cases which is the average of the irds for each case.

## Functions

- `print(sc_ird)`: Print results

- `export(sc_ird)`: Export results to html

## References

Parker, R. I., Vannest, K. J., & Brown, L. (2009). The improvement rate
difference for single-case research. Exceptional Children, 75(2),
135-150.

Parker, R. I., Vannest, K. J., & Davis, J. L. (2011). Effect Size in
Single-Case Research: A Review of Nine Nonoverlap Techniques. Behavior
Modification, 35(4), 303-322. https://doi.org/10.1177/0145445511399147

Pustejovsky, J. E. (2019). Procedural sensitivities of effect sizes for
single-case designs with directly observed behavioral outcome measures.
*Psychological Methods*, *24(2)*, 217-235.
https://doi.org/10.1037/met0000179

## See also

Other overlap functions:
[`nap()`](https://jazznbass.github.io/scan/reference/nap.md),
[`overlap()`](https://jazznbass.github.io/scan/reference/overlap.md),
[`pand()`](https://jazznbass.github.io/scan/reference/pand.md),
[`pem()`](https://jazznbass.github.io/scan/reference/pem.md),
[`pet()`](https://jazznbass.github.io/scan/reference/pet.md),
[`pnd()`](https://jazznbass.github.io/scan/reference/pnd.md),
[`print.sc_cdc()`](https://jazznbass.github.io/scan/reference/cdc.md),
[`select_phases()`](https://jazznbass.github.io/scan/reference/select_phases.md),
[`tau_u()`](https://jazznbass.github.io/scan/reference/tau_u.md)

# Percentage of non-overlapping data (PND)

This function returns the percentage of non-overlapping data. Due to its
error-proneness the PND should not be used, but
[`nap`](https://jazznbass.github.io/scan/reference/nap.md) or
[`pand`](https://jazznbass.github.io/scan/reference/pand.md) instead
(see Parker & Vannest, 2009).

## Usage

``` r
pnd(data, dvar, pvar, decreasing = FALSE, phases = c(1, 2))
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

- PND:

  Percentage of non-overlapping data.

## Details

PND is calculated by determining the number of data points in phase B
that exceed the highest data point in phase A (or are lower than the
lowest data point in phase A for decreasing data) divided by the total
number of data points in phase B. This value is then multiplied by 100
to get a percentage value.

## See also

Other overlap functions:
[`ird()`](https://jazznbass.github.io/scan/reference/ird.md),
[`nap()`](https://jazznbass.github.io/scan/reference/nap.md),
[`overlap()`](https://jazznbass.github.io/scan/reference/overlap.md),
[`pand()`](https://jazznbass.github.io/scan/reference/pand.md),
[`pem()`](https://jazznbass.github.io/scan/reference/pem.md),
[`pet()`](https://jazznbass.github.io/scan/reference/pet.md),
[`print.sc_cdc()`](https://jazznbass.github.io/scan/reference/cdc.md),
[`select_phases()`](https://jazznbass.github.io/scan/reference/select_phases.md),
[`tau_u()`](https://jazznbass.github.io/scan/reference/tau_u.md)

## Author

Juergen Wilbert

## Examples

``` r
## Calculate the PND for multiple single-case data
pnd(GruenkeWilbert2014)
#> Percent Non-Overlapping Data
#> 
#>    Case  PND Total Exceeds
#>   Anton 100%    14      14
#>     Bob 100%    11      11
#>    Paul 100%    12      12
#>  Robert 100%    10      10
#>     Sam 100%    13      13
#>     Tim 100%    14      14
#> 
#> Mean  : 100 %
```

# Standardized mean differences for single-case data

The `smd()` function provides various standardized mean effect sizes for
single-case data.

## Usage

``` r
smd(data, dvar, pvar, phases = c(1, 2))

# S3 method for class 'sc_smd'
print(x, digits = "auto", ...)

# S3 method for class 'sc_smd'
export(
  object,
  caption = NA,
  footnote = NA,
  filename = NA,
  select = c("Case", `Mean A` = "mA", `Mean B` = "mB", `SD A` = "sdA", `SD B` = "sdB",
    `SD Cohen` = "sd cohen", `SD Hedges` = "sd hedges", "Glass' delta", "Hedges' g",
    "Hedges' g correction", "Hedges' g durlak correction", "Cohen's d"),
  round = 2,
  decimals = 2,
  flip = FALSE,
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

- phases:

  A vector of two characters or numbers indicating the two phases that
  should be compared. E.g., `phases = c("A","C")` or `phases = c(2,4)`
  for comparing the second to the fourth phase. Phases could be combined
  by providing a list with two elements. E.g.,
  `phases = list(A = c(1,3), B = c(2,4))` will compare phases 1 and 3
  (as A) against 2 and 4 (as B). Default is `phases = c(1,2)`.

- x:

  Object

- digits:

  The minimum number of significant digits to be use. If set to "auto"
  (default), values are predefined.

- ...:

  Further arguments passed to the function.

- object:

  An scdf or an object exported from a scan function.

- caption:

  Character string with table caption. If left NA (default) a caption
  will be created based on the exported object.

- footnote:

  Character string with table footnote. Several strings are combined
  into a footnote of several lines. If left NA (default) a footnote will
  be created based on the exported object. `NULL` or `""` suppress the
  footnote.

- filename:

  String containing the file name. If a filename is given the output
  will be written to that file.

- select:

  A character vector containing the names of the variables to be
  included. If the vector is named, the variables will be renamed
  accordingly.

- round:

  Integer passed to the digits argument used to round values.

- decimals:

  Decimal places that are reported.

- flip:

  If TRUE, some objects are exported with rows and columns flipped.

## Details

It computes 'Cohen's d', 'Hedges' g', 'Hedges' g correction', 'Hedges' g
durlak correction', and 'Glass' delta' for each single-case included in
an scdf.

'sd cohen' is the (unweigted) average of the variance of phase A and B.
'sd Hedges' is the weighted average of the variance of phase A and B
(with a degrees of freedom correction). 'Hedges' g' is the mean
difference divided by 'sd Hedges'. 'Hedges' g correction' and 'Hedges' g
durlak correction' are two approaches of correcting Hedges' g for small
sample sizes. 'Glass' delta' is the mean difference divided by the
standard deviation of the A-phase. 'Cohens d' is the mean difference
divided by 'sd cohen'.

## Functions

- `print(sc_smd)`: Print results

- `export(sc_smd)`: Export results as html table (see
  [`export()`](https://jazznbass.github.io/scan/reference/export.md))

## See also

[`overlap()`](https://jazznbass.github.io/scan/reference/overlap.md),
[`describe()`](https://jazznbass.github.io/scan/reference/describe.md)

## Author

Juergen Wilbert

## Examples

``` r
smd(exampleAB)
#> Standardized mean differences
#> 
#>                             Johanna Karolina  Anja
#> mA                            54.60    51.80 53.60
#> mB                            74.13    73.47 74.07
#> sdA                            2.41     6.83  3.05
#> sdB                            8.94     9.76  7.57
#> sd cohen                       6.55     8.43  5.77
#> sd hedges                      7.97     9.19  6.83
#> Glass' delta                   8.11     3.17  6.71
#> Hedges' g                      2.45     2.36  3.00
#> Hedges' g correction           2.35     2.26  2.87
#> Hedges' g durlak correction    2.23     2.14  2.72
#> Cohen's d                      2.98     2.57  3.54
#> 
```

# Dummy function to inherit global descriptions of parameters

This function is only used to inherit parameter descriptions in the
documentation of other functions. It has no other purpose and is not
meant to be called directly.

## Usage

``` r
.inheritParams(
  data,
  scdf,
  dvar,
  mvar,
  pvar,
  decreasing,
  phases,
  model,
  contrast,
  contrast_level,
  contrast_slope,
  trend,
  level,
  slope,
  nice,
  ...
)
```

## Arguments

- data:

  A single-case data frame. See
  [`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md) to
  learn about this format.

- scdf:

  A single-case data frame. See
  [`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md) to
  learn about this format.

- dvar:

  Character string with the name of the dependent variable. Defaults to
  the attributes in the scdf file.

- mvar:

  Character string with the name of the measurement time variable.
  Defaults to the attributes in the scdf file.

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

- model:

  Model used for calculating the dummy parameters (see Huitema & McKean,
  2000). Default is `model = "W"`. Possible values are: `"B&L-B"`,
  `"H-M"`, `"W"`, and deprecated `"JW"`.

- contrast:

  Sets contrast_level and contrast_slope. Either "first", "preceding" or
  a contrast matrix. If NA contrast is ignored.

- contrast_level:

  Either "first", "preceding" or a contrast matrix. If NA contrast_level
  is a copy of contrast.

- contrast_slope:

  Either "first", "preceding" or a contrast matrix. If NA contrast_level
  is a copy of contrast.

- trend:

  A logical indicating if a trend parameters is included in the model.

- level:

  A logical indicating if a level parameters is included in the model.

- slope:

  A logical indicating if a slope parameters is included in the model.

- nice:

  If set TRUE (default) output values are rounded and optimized for
  publication tables.

- ...:

  Further arguments passed to the function.

## Value

No return value.

## Author

Juergen Wilbert

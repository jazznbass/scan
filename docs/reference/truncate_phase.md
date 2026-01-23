# Truncate single-case data

\#' *This function is superseded by the more versatile
[`transform.scdf`](https://jazznbass.github.io/scan/reference/transform.scdf.md)
function.* This function truncates data points at the beginning and / or
end of each phase in each case.

## Usage

``` r
truncate_phase(
  data,
  dvar,
  pvar,
  truncate = list(A = c(0, 0), B = c(0, 0)),
  na = TRUE
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

- truncate:

  A list with a vector of two (beginning and end) values for each phase
  defining the number of data points to be deleted. For lists of
  single-case data frames, the truncation is adapted to the length of
  each phase for each single case.

- na:

  If FALSE, the truncated measurement times are deleted. If TRUE, NAs
  are set for the dependent variable.

## Value

A truncated data frame (for each single-case).

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
[`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md),
[`select_cases()`](https://jazznbass.github.io/scan/reference/select_cases.md),
[`set_vars()`](https://jazznbass.github.io/scan/reference/set_vars.md),
[`shift()`](https://jazznbass.github.io/scan/reference/shift.md),
[`smooth_cases()`](https://jazznbass.github.io/scan/reference/smooth_cases.md),
[`standardize()`](https://jazznbass.github.io/scan/reference/standardize.md)

## Author

Juergen Wilbert

## Examples

``` r
## Truncate the first two data points of both phases and compare the two 
## data sets
study <- c(
  "Original" = byHeart2011[1],
  "Selected" = truncate_phase(
    byHeart2011[1], truncate = list(A = c(2, 0), B = c(2, 0))
  )
)
#> Deletet measurements per case:
#> 
#> Lisa (Turkish): 1 2 6 7
plot(study)
#> Warning: This function is deprecated. It might be dropped without any further notice in a future update of scan.
#> Please use function 'scplot' from the package 'scplot' instead of 'plot'.
#> Warning: This function is deprecated. It might be dropped without any further notice in a future update of scan.
#> Please use function 'scplot' from the package 'scplot' instead of 'style_plot'.
```

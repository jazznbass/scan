# as_scdf

Converts a data frame to an scdf object.

## Usage

``` r
as_scdf(
  object,
  cvar = "case",
  pvar = "phase",
  dvar = "values",
  mvar = "mt",
  phase_names = NULL,
  sort_cases = FALSE
)
```

## Arguments

- object:

  A data.frame

- cvar:

  Sets the "case" variable. Defaults to `case`.

- pvar:

  Sets the "phase" variable. Defaults to `phase`.

- dvar:

  Sets the "values" variable. Defaults to `values`.

- mvar:

  Sets the variable name of the "mt" variable. Defaults to `mt`.

- phase_names:

  A character vector with phase names. Defaults to the phase names
  provided in the phase variable.

- sort_cases:

  If set TRUE, the resulting list is sorted by label names
  (alphabetically increasing).

## Value

An scdf.

## See also

Other data manipulation functions:
[`add_l2()`](https://jazznbass.github.io/scan/reference/add_l2.md),
[`as.data.frame.scdf()`](https://jazznbass.github.io/scan/reference/as.data.frame.scdf.md),
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
[`standardize()`](https://jazznbass.github.io/scan/reference/standardize.md),
[`truncate_phase()`](https://jazznbass.github.io/scan/reference/truncate_phase.md)

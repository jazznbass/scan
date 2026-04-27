# Changelog

## scan 0.68.1

- Introduced rlang error and messaging system for more informative error
  messages and warnings.

## scan 0.68.0

CRAN release: 2026-04-01

### New features

- Reworked
  [`fill_missing()`](https://jazznbass.github.io/scan/reference/fill_missing.md)
  function. It now interpolates the values for all variables in an scdf.

### Shinyscan app

- Introduced tabset in the data load and example data tab.
- Tabsets in the plot dataset with many added options.
- Tabsets in the stats tab with Arguments on a separate tab.

### Corrections

- Extended roxygen helpfiles for many functions.

### Fix

- export function for
  [`plm()`](https://jazznbass.github.io/scan/reference/plm.md) does not
  throw an error when AIC can not be computed.

### New example datasets

- example_stranger: example for screen time of Stranger Things
  characters.
- example_atd: three case example for AB alternating treatment design.

## scan 0.67.0

CRAN release: 2025-09-11

### Reworked Shiny Scan app

- Complete new design for the “plot” tab.

### New features

- [`shinyscan()`](https://jazznbass.github.io/scan/reference/shinyscan.md):
  New argument theme (default is `cerulean`) that allows to run
  shinyscan in a different bootstrap 5 theme. I like
  `shinyscan(theme = "united")`
- [`export()`](https://jazznbass.github.io/scan/reference/export.md)
  methods for:
  [`cdc()`](https://jazznbass.github.io/scan/reference/cdc.md),
  [`corrected_tau()`](https://jazznbass.github.io/scan/reference/corrected_tau.md),
  [`rand_test()`](https://jazznbass.github.io/scan/reference/rand_test.md),
  [`ird()`](https://jazznbass.github.io/scan/reference/ird.md),
  [`outlier()`](https://jazznbass.github.io/scan/reference/outlier.md),
  [`autocorr()`](https://jazznbass.github.io/scan/reference/autocorr.md).

## scan 0.66.0

CRAN release: 2025-09-02

### New features

- [`as_scdf()`](https://jazznbass.github.io/scan/reference/as_scdf.md):
  Add format checks with informative error messages.  
  (Mainly useful for externally loaded files via
  [`read_scdf()`](https://jazznbass.github.io/scan/reference/read_scdf.md).)
- [`shinyscan()`](https://jazznbass.github.io/scan/reference/shinyscan.md):
  New argument `browser`. Default `"external"` launches the Shiny app in
  an external browser.
- [`between_smd()`](https://jazznbass.github.io/scan/reference/between_smd.md):
  - Stops if the `scdf` contains only one case.  
  - Redesigned outputs.  
  - Improved Bayesian analysis.  
  - Added confidence/credible intervals and new argument `ci`.
- [`hplm()`](https://jazznbass.github.io/scan/reference/hplm.md):
  - Argument `ar` sets a auto-regression parameter for correlated
    residuals.
  - Argument `unequal_variances` weights the estimations by within phase
    variances.

### New shiny app for importing data

- [`import_scdf()`](https://jazznbass.github.io/scan/reference/import_scdf.md):
  opens a small import menu. Also available as an addin in R Studio.

### Reworked Shiny Scan app

- Redesign the “Add case” workflow.
- Added an import procedure to choose variable names at import.
- Now it is possible to start shinyscan with an scdf object and directly
  start to analyse it `shinyscan(exampleABC)`.
- Stats tab: auto-fill the “Output arguments” field when a statistics
  function is selected.
- Stats tab/ Settings tab: “Description” switch in settings can be set
  to provide additional short descriptions for each stats function.
- Switch to Bootstrap 5.
- Miscellaneous visual polish.

### Error correction

- Addin in R-Studio is now renames to “Lauch Shiny-Scan” and finally
  works.

## scan 0.65.1

CRAN release: 2025-07-13

### Error correction

- [`plm()`](https://jazznbass.github.io/scan/reference/plm.md): dummy
  slope values did falsely ignore missing values. That could lead to
  incorrect slope effect estimations.

## scan 0.65.0

CRAN release: 2025-06-29

### New function

- [`rowwise()`](https://jazznbass.github.io/scan/reference/transform.scdf.md):
  A new helper function for
  [`transform()`](https://rdrr.io/r/base/transform.html) that allows to
  make calculations rowwise:

``` .r
ex <- exampleAB_add; ex[[1]]$wellbeing[c(3, 6)] <- NA
transform(
  ex, 
  mean_dv = rowwise(mean(c(wellbeing, cigarrets, depression), na.rm = TRUE))
)
```

### New features

- [`mplm()`](https://jazznbass.github.io/scan/reference/mplm.md):
  Reworked the function and its output. Now provides global F test and a
  more integrated depiction of the coefficients. Added export() method.
- `plm()/print.sc_plm()`: either print partial or delta (incremental),
  or both R squared:

``` r

plm(exampleAB$Johanna) |> 
  print(r_squared = c("delta", "partial"))
```

### Corrections

- [`plm()`](https://jazznbass.github.io/scan/reference/plm.md):
  Corrected calculation for model fit F statistic and R2 for models
  without an intercept
- [`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md):
  Reported a false error message when the `phase.start` argument was
  used and mt started with 0.

### Further changes

- [`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md):
  Optimized code, function arguments, and help page.

## scan 0.64.0

CRAN release: 2025-03-19

### New functions

- [`fetch()`](https://jazznbass.github.io/scan/reference/fetch.md):
  General getter function to extract components from a scan object. It
  takes a scan object and and an optional argument and returns
  sub-objects. For now, it extracts the regression object of class glm,
  lm, lme from the respective plm, hplm, and mplm objects:

``` r

mod <- plm(exampleAB$Johanna)
fetch(mod)
```

### New features

- [`anova()`](https://rdrr.io/r/stats/anova.html): Implemented
  functionality of further arguments of the generic anova functions.
- `options(scan.string.dummy.phase = "phase")`: can be renamed to avoid
  name conflicts in the output of regression models.
- `options(scan.string.dummy.slope = "inter")`: can be renamed to avoid
  name conflicts in the output of regression models.

## scan 0.63.0

CRAN release: 2025-03-01

### New functions

- [`bplm()`](https://jazznbass.github.io/scan/reference/bplm.md):
  Bayesian piecewise regression model. Applies a Markov Chain Monte
  Carlo sampler from the MCMCglmm package. With export method.
- [`add_dummy_variables()`](https://jazznbass.github.io/scan/reference/add_dummy_variables.md):
  Helper function that adds dummy variables necessary to calculate a plm
  to an scdf.
- `anova.plm() anova.hplm()`: Methods for likelihood ratio model
  comparison.

### New features

- [`hplm()`](https://jazznbass.github.io/scan/reference/hplm.md): Adds
  inter correlation of random variables to the print output.
- new option: `scan.rename.predictors` can be set to `no`, `concise`, or
  `full`. Changes how predictors of regression models are renamed.
- [`between_smd()`](https://jazznbass.github.io/scan/reference/between_smd.md):
  Added support for Bayesian regressions `model = "bayesian"` or
  providing an object returned from the
  [`bplm()`](https://jazznbass.github.io/scan/reference/bplm.md)
  function.

### Solved bugs

- [`plm()`](https://jazznbass.github.io/scan/reference/plm.md): setting
  `var_trials` to a constant threw an error.

### Corrections / Changes

- [`between_smd()`](https://jazznbass.github.io/scan/reference/between_smd.md):
  Reworked the function output to avoid confusion. A ‘pure’ between case
  smd estimation is provided when the argument
  `include_residuals = FALSE` is set.
- `print.sc_plm() / export.sc_plm()`: New argument `ci` for specifying a
  confidence interval. Either `FALSE`, `TRUE` or a number between 0 and
  1 (0.90 for a 90% intervals).

## scan 0.62.0

CRAN release: 2025-02-05

### New functions / features

- [`rescale()`](https://jazznbass.github.io/scan/reference/rescale.md):
  New function as a helper for getting standardized estimators in
  regression models. e.g. `exampleAB |> rescale() |> hplm()`.
- [`between_smd()`](https://jazznbass.github.io/scan/reference/between_smd.md):
  Calculates between case standardized mean differences as proposed by
  Pustejovsky et. aL (2014). Can take complex hplm models as a basis.
- [`na.omit.scdf()`](https://jazznbass.github.io/scan/reference/na.omit.scdf.md):
  scdf method for generic
  [`na.omit()`](https://rdrr.io/r/stats/na.fail.html). Removes any row
  with a missing value from an scdf.

### New features

- [`design()`](https://jazznbass.github.io/scan/reference/design.md):
  Argument `random_start_values` randomly assigns start values for each
  case based on the distribution (`normal`, `poisson` or `binomial`) and
  the respective parameters (`start_values`, `s`, `n_trials`).
- [`print.sc_hplm()`](https://jazznbass.github.io/scan/reference/hplm.md):
  New argument `smd`. If set TRUE, between case smd results are
  reported.
- [`tau_u()`](https://jazznbass.github.io/scan/reference/tau_u.md): New
  method `"tarlow"` calculates Tau-U as implemented in an R code and
  online calculator by Tarlow (2017). Here, tau values are calculated as
  in the
  `method = "complete", continuity_correction = TRUE, tau_method = "a"`.
  Inferential statistics are calculated based on tau b and the standard
  deviation for S is derived directly from Kendall’s Tau B analysis
  (different from the `parker` and `complete` methods).
- [`rand_test()`](https://jazznbass.github.io/scan/reference/rand_test.md):
  It is now possible to provide new functions for calculating the
  statistic directly with a list to the `statistic_function` argument.
  This list must have an element named `statistic` with a function that
  takes two arguments `a` and `b` and returns a single numeric value. A
  second element of the list is named `aggregate` which takes a function
  with one numeric argument that returns a numeric argument. This
  function is used to aggregate the values of a multiple case design. If
  you do not provide this element, it uses the default
  `function(x) sum(x)/length(x)`. The third optional argument is `name`
  which provides a name for your user function.

``` r

userstat <- list(
  statistic = function(a, b) median(b) - median(a), 
  aggregate = function(x) median(x),
  name = "median B - A"
)

rand_test(exampleAB, statistic_function = userstat , complete = TRUE)

# which is identical to:
rand_test(exampleAB, statistic = "Median B-A" , complete = TRUE)
```

- [`rand_test()`](https://jazznbass.github.io/scan/reference/rand_test.md):
  Returns startpoints for the random permutations.
- [`plot_rand()`](https://jazznbass.github.io/scan/reference/plot_rand.md):
  New argument `type` when `"xy"` a plot with splitpoints and statistics
  is drawn. This allows to see graphically at which measurement time a
  statistic changes.

``` r

Leidig2018[4] |> 
  na.omit() |> 
  rand_test(complete = TRUE, limit = 1, statistic = "SMD glass") |> 
  plot_rand(type = "xy")
```

### Corrections / Changes

- [`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md): Throws
  an error when argument `phase_starts` is set and the beginning of the
  first phase is not the first measurement.
- [`tau_u()`](https://jazznbass.github.io/scan/reference/tau_u.md):
  Method `"parker"` ignores the `tau_method` setting and sets
  `continuity_correction = FALSE`. This follows the Parker (2011) paper.
  There, the inferential statistics are calculated using Kendall’s Tau b
  while the actual Tau calculation applies Kendall’s Tau a (without
  ties).
- [`rand_test()`](https://jazznbass.github.io/scan/reference/rand_test.md):
  Missing values in the dependent variable are now removed before
  calculations.

### Solved bugs

## scan 0.61.0

CRAN release: 2024-07-01

### Solved bugs

- Corrected the name of the level-effect predictor for regressions when
  the phase variable is not named “phase”.

### New

- Tip-of-the-day like message at start-up.
- Multiple improvements of the Shiny app (try out with
  [`shinyscan()`](https://jazznbass.github.io/scan/reference/shinyscan.md))
- new output engine for rendering html export based on *gt table*. Set
  `options(scan.export.engine = "gt")`. This engine allows to export
  tables into docx format:
  `overlap(exampleAB) |> export(file = "test.docx", flip = TRUE)`.
- new export functions for
  [`pem()`](https://jazznbass.github.io/scan/reference/pem.md),
  [`pet()`](https://jazznbass.github.io/scan/reference/pet.md),
  [`pnd()`](https://jazznbass.github.io/scan/reference/pnd.md), and
  [`summary()`](https://rdrr.io/r/base/summary.html) (either
  `summary(exampleAB) |> export()` or
  `export(exampleAB, summary = TRUE)`)

### Changes

- [`rci()`](https://jazznbass.github.io/scan/reference/rci.md): removed
  the Hageman et al. method as it is not appropriate for single-cases in
  the current implementation.
- [`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md): New
  argument `phase_starts()`. Which defines the measurement times of the
  start of each phase. `phase_starts = list(A = 1, B = 10, C = 15)`. It
  throws an error when a phase start is defined where no corresponding
  measurement-time exists. `phase_starts` is a generalization of
  `B_start`.
- [`rand_test()`](https://jazznbass.github.io/scan/reference/rand_test.md):
  New option for `statistic`: `SMD` calculates the standardized mean
  difference as Hedge’s g with Durlak correction. `W-test` computes
  Wilcoxon tests and compares average W statistics. `T-test` computes
  T-tests and compares average t-Values. `NAP` and `NAP decreasing` for
  Non-overlap of all pairs.
- [`nap()`](https://jazznbass.github.io/scan/reference/nap.md): added
  Cohen’s d and R-Squared effects.
- [`export()`](https://jazznbass.github.io/scan/reference/export.md):
  `select` argument for `nap`
- [`coef.sc_hplm()`](https://jazznbass.github.io/scan/reference/hplm.md):
  new `casewise` argument. If set TRUE, returns the effect estimations
  casewise.
- [`print.sc_hplm()`](https://jazznbass.github.io/scan/reference/hplm.md):
  new `casewise` argument. If set TRUE, returns the effect estimations
  casewise.
- [`export.sc_hplm()`](https://jazznbass.github.io/scan/reference/hplm.md):
  new `casewise` argument. If set TRUE, returns the effect estimations
  casewise.
- [`export.scdf()`](https://jazznbass.github.io/scan/reference/export.md):
  new `summary` argument. If TRUE, returns a summary.
- [`hplm()`](https://jazznbass.github.io/scan/reference/hplm.md): new
  arguments `random_trend`, `random_level`, and `random_slope` to
  selectively add respective random slope effects to the model.

### New examples

- `Parker2009b`

## scan 0.60.0

CRAN release: 2023-08-08

### New function

- [`ird()`](https://jazznbass.github.io/scan/reference/ird.md): Robust
  improvement rate difference as formulated by Postejovski (2019).

### Changes

- [`pand()`](https://jazznbass.github.io/scan/reference/pand.md):
  Rewrote function. New argument `method` allows to apply the sorting
  algorithm proposed in Parker 2007 and `method = "minimum"` applies the
  exact method provided by Pustejovski in 2019. Furthermore, the tau
  test was replaced with a X Squared and a Fisher exact test.
- `corrected_tau`: Report results with warning when all phase A data are
  identical; new argument tau_method = “a” to switch to Kendall’s tau-a.
- [`export()`](https://jazznbass.github.io/scan/reference/export.md):
  new for
  [`pand()`](https://jazznbass.github.io/scan/reference/pand.md).
- New example datasets: Tarlow2017, Parker2011b, Parker2009, Parker2007

### Shiny scan

- Added power-analyses
- Added settings
- Extended save options
- Various changes and optimization of the ui

## scan 0.59.0

CRAN release: 2023-06-03

- [`describe()`](https://jazznbass.github.io/scan/reference/describe.md):
  now works correctly for duplicated phase names.
- shiny-app no longer depends on `shinyjs` and `markdown`.
  [`shinyscan()`](https://jazznbass.github.io/scan/reference/shinyscan.md)
  asks if missing packages `scplot` and `shiny` should be installed
  automatically.

## scan 0.58

CRAN release: 2023-05-24

### Shiny app added

- start the app with
  [`shinyscan()`](https://jazznbass.github.io/scan/reference/shinyscan.md)
- you need the following packages to run the app:
  - `shiny`, `shinyjs`, `scplot`, `markdown`

### New fucntions

- [`batch_apply()`](https://jazznbass.github.io/scan/reference/batch_apply.md):
  Apply a function to each element in an scdf. Use `.` as a placeholder
  for the scdf case.

``` r

batch_apply(exampleAB, plm(.) |> coef())
```

- Helper functions for
  [`transform()`](https://rdrr.io/r/base/transform.html): `n()`,
  [`all_cases()`](https://jazznbass.github.io/scan/reference/transform.scdf.md),
  [`across_cases()`](https://jazznbass.github.io/scan/reference/transform.scdf.md),
  `first_of`
- Helper functions for
  [`transform()`](https://rdrr.io/r/base/transform.html):
  [`moving_mean()`](https://jazznbass.github.io/scan/reference/transform.scdf.md),
  [`moving_median()`](https://jazznbass.github.io/scan/reference/transform.scdf.md),
  [`local_regression()`](https://jazznbass.github.io/scan/reference/transform.scdf.md)
- [`corrected_tau()`](https://jazznbass.github.io/scan/reference/corrected_tau.md):
  returns multiple cases
- [`export()`](https://jazznbass.github.io/scan/reference/export.md):
  new for `nap(), pen()`
- `c()/combine()`: new arguments to set author and info attributes of
  the resulting scdf (`author`, `info`)

### Changes in functions

- [`plm()`](https://jazznbass.github.io/scan/reference/plm.md): Print
  function allows to set maximum lag for autocorrelations; Overall
  significance Ljung_Box test is reported.

``` r

plm(exampleAB$Johanna) |> print(lag_max = 5)
```

### Bug fixes

- solved: [`summary()`](https://rdrr.io/r/base/summary.html) failed when
  one of the cases had no name
- solved: [`nap()`](https://jazznbass.github.io/scan/reference/nap.md)
  only reported values for the first case with multiple cases.
- solved:
  [`add_l2()`](https://jazznbass.github.io/scan/reference/add_l2.md)
  lost column name of l2 variable when l2 had only one variable.

### minor

- [`convert()`](https://jazznbass.github.io/scan/reference/convert.md):
  new arguments. indent sets the indentation. When the scdf contains
  only one case, no study is combined.
- [`select_phases()`](https://jazznbass.github.io/scan/reference/select_phases.md):
  New argument `phase_names` sets names of the recombined phases. The
  default `"auto"` creates combinations of the phase names automatically
  (e.g., `A = c("A", "B", B = "C")` results in phases `AB` and `C`).

## scan 0.56

CRAN release: 2023-02-16

### New features

- [`export()`](https://jazznbass.github.io/scan/reference/export.md):
  New export for
  [`power_test()`](https://jazznbass.github.io/scan/reference/power_test.md)
  and [`smd()`](https://jazznbass.github.io/scan/reference/smd.md)
  output.

- [`export()`](https://jazznbass.github.io/scan/reference/export.md):
  [`tau_u()`](https://jazznbass.github.io/scan/reference/tau_u.md)
  export with new argument `case` which takes the values `"meta"` or
  `"all"` and new argument `select` allowing to select, reorder, and
  rename specific variables.

- [`select_cases()`](https://jazznbass.github.io/scan/reference/select_cases.md):
  Allow for a selection based on object names (like in substitute).

``` r

select_cases(exampleAB, -c(Johanna, Karolina))
select_cases(exampleAB, Johanna, Karolina)
v <- c("Moritz", "Jannis")
select_cases(exampleA1B1A2B2, v)
```

- [`plm()`](https://jazznbass.github.io/scan/reference/plm.md),
  [`hplm()`](https://jazznbass.github.io/scan/reference/hplm.md): New
  arguments `contrast_level` and `contrast_slope` allow for setting the
  contrasts for level and slope separately. Both elements can either be
  “first” or “preceding”.
- Speed-up
  [`tau_u()`](https://jazznbass.github.io/scan/reference/tau_u.md) by
  20%.
- rewrote
  [`as_scdf()`](https://jazznbass.github.io/scan/reference/as_scdf.md),
  [`read_scdf()`](https://jazznbass.github.io/scan/reference/read_scdf.md).
  Now it is easier to import data from any file format.

``` r

readODS::read_ods("filename.ods") |> as.scdf()
openxlsx::read.xlsx("filename.xlsx") |> as.scdf()


readODS::read_ods("filename.ods") |> 
  as.scdf(
    cvar = "id", 
    pvar = "section", 
    mvar = "day", 
    phase_names = c("baseline", "intervention")
  )
  
as.data.frame(exampleABC) |> readODS::write_ods("filename.xlsx")
as.data.frame(exampleABC) |> openxlsx::write.xlsx("filename.xlsx")
```

### Bug fixes

- [`tau_u()`](https://jazznbass.github.io/scan/reference/tau_u.md):
  Rewrote the calculation of meta analyses and confidence intervals.
- `tau_u(), correted_tau()`: corrected a wrong calculation of the
  continuity correction when values where lower in phase B.
- [`tau_u()`](https://jazznbass.github.io/scan/reference/tau_u.md):
  Implemented a new method for calculating confidence intervals based on
  Fisher-Z transformations (see Long, J. D., & Cliff, N. (1997).
  Confidence intervals for Kendall’s tau. British Journal of
  Mathematical and Statistical Psychology, 50(1), 31-41.

### superseded function

- `smooth_caes()`,
  [`shift()`](https://jazznbass.github.io/scan/reference/shift.md),
  `standardise()`,
  [`ranks()`](https://jazznbass.github.io/scan/reference/ranks.md),
  `truncate_phases()`: All superseded by
  [`transform()`](https://rdrr.io/r/base/transform.html) and its helper
  functions. See details in the help files of transform and in the
  scan-book.

### minor changes

- `as.data.frane.scdf()`/[`as_scdf()`](https://jazznbass.github.io/scan/reference/as_scdf.md):
  keep and retrieve scdf attributes.

## scan 0.55

CRAN release: 2022-08-29

### new functions

- `coef()/ coefficients()`: Method for base R `coef` function for
  plm/hplm objects. Extracts coefficient tables from provided object.

``` .r
coef(plm(exampleAB$Johanna))
```

### New features

- [`power_test()`](https://jazznbass.github.io/scan/reference/power_test.md):
  New argument `ci` provides confidence intervals for power, alpha
  error, and correct proportions. New arguments
  `binom_test_alpha, binom_test_power, binom_test_correct` provide test
  against a provided proportion for alpha, power, and correct
  proportions.

``` .r
design <- design(
  n = 1, phase_design = list(A = 6, B = 9),
  rtt = 0.8, level = 1.0, trend = 0.05
)

power_test(
  design, ci = 0.95, binom_test = TRUE
)
```

- [`plm()`](https://jazznbass.github.io/scan/reference/plm.md),
  [`hplm()`](https://jazznbass.github.io/scan/reference/hplm.md): Added
  contrast argument with values `"first"` or “`preceding`”. Now
  `model = "JW"` is deprecated and identical to
  `model = "B&L-B", contrast = "preceding"`.  
- [`plm()`](https://jazznbass.github.io/scan/reference/plm.md),
  [`hplm()`](https://jazznbass.github.io/scan/reference/hplm.md): Added
  model `"W"`. Which shifts the measurement-time variable to start with
  zero. This leads to a more sensible estimation of the intercept (where
  the intercept depicts the estimated score at the start). `model = "W"`
  is now the default. Use `model = "B&L-B"` for previous defaults.

### Bug fixes

- solved [\#66](https://github.com/jazznbass/scan/issues/66):
  [`set_vars()`](https://jazznbass.github.io/scan/reference/set_vars.md)
  working.
- [`read_scdf()`](https://jazznbass.github.io/scan/reference/read_scdf.md)
  now works when cvar is not the first column.

### Changes to functions

- [`corrected_tau()`](https://jazznbass.github.io/scan/reference/corrected_tau.md):
  changed default setting of arguments to:
  `repeated = FALSE, continuity = FALSE`. Now the default results match
  the calculator developed by Tarlow.

- [`tau_u()`](https://jazznbass.github.io/scan/reference/tau_u.md):
  Implemented a continuity_correction (S-1 for calculating Z)

## scan 0.54.1

CRAN release: 2022-04-03

### Bug fixes

- [`select_phases()`](https://jazznbass.github.io/scan/reference/select_phases.md)
  now works when phase variable is not “phase”
- [`estimate_design()`](https://jazznbass.github.io/scan/reference/estimate_design.md)
  was broken and is fixed now.
- [`print()`](https://rdrr.io/r/base/print.html) function for
  `sc_design`fixed.

## scan 0.54

CRAN release: 2022-03-23

### new functions

- [`transform()`](https://rdrr.io/r/base/transform.html): Takes an scdf
  and calculates or modifies variables for each case
  (`transform(exampleAB, z_values = scale(values), t_values = 50 + z_values * 10)`).
- [`smd()`](https://jazznbass.github.io/scan/reference/smd.md) reporting
  various types of standardized mean differences.

### reanmed functions (old functionnames still work)

- [`readSC()`](https://jazznbass.github.io/scan/reference/deprecated-functions.md)
  -\>
  [`read_scdf()`](https://jazznbass.github.io/scan/reference/read_scdf.md)
- [`writeSC()`](https://jazznbass.github.io/scan/reference/deprecated-functions.md)
  -\>
  [`write_scdf()`](https://jazznbass.github.io/scan/reference/write_scdf.md)
- `design_rSC()` -\>
  [`design()`](https://jazznbass.github.io/scan/reference/design.md)
- [`rSC()`](https://jazznbass.github.io/scan/reference/deprecated-functions.md)
  -\>
  [`random_scdf()`](https://jazznbass.github.io/scan/reference/random_scdf.md)

### Complete rework - as new

- [`power_test()`](https://jazznbass.github.io/scan/reference/power_test.md)
  with various extensions, optimizations, and solved various bugs.
  rewrote the `print` method, added an argument `duration` to print the
  computation duration. Added the `'n_trials'` argument for binomial
  distributions. Extended the help page.  
- [`design()`](https://jazznbass.github.io/scan/reference/design.md) and
  its print method. Extended the help page. Rewrote the algorithm for
  the ‘binomial’ distribution.

### Extended functions

- [`plm()`](https://jazznbass.github.io/scan/reference/plm.md): rewrote
  the analysis function for binomial tests. These now need an argument
  `var_trials` to define the number of trials per measurement. The
  `dvar_percentage` argument must be set TRUE when the dependent
  variables are percentages (and `family = 'binomal'`).
- speed optimized
  [`random_scdf()`](https://jazznbass.github.io/scan/reference/random_scdf.md).
  Rewrote the algorithm for ‘poisson’ distributed measures. Rewrote the
  algorithm for the ‘binomial’ distribution. Extended the help page.  
- [`read_scdf()`](https://jazznbass.github.io/scan/reference/read_scdf.md):
  extracts filetype from file extension.
- [`read_scdf()`](https://jazznbass.github.io/scan/reference/read_scdf.md):
  New `yaml` import options for scdf files

``` yml
Anna:
  values:
    A: [1, 3, 4, 5, 6, 7]
    B: [8, 9, 10, 10, 11]

Toni:
  values:
    A: [2, 3, 4, 5, 6, 7]
    B: [3, 9, 10, 10,11]
  control_var: [1,2,3,4,5,6,7,8,1,2,3]
```

- [`tau_u()`](https://jazznbass.github.io/scan/reference/tau_u.md)
  [\#51](https://github.com/jazznbass/scan/issues/51): Added option for
  confidence intervals for tau_u output.

### Solved error in functions

- [`describe()`](https://jazznbass.github.io/scan/reference/describe.md):
  solved wrong calculation of Hedges G when phase length differed.
- [`plm()`](https://jazznbass.github.io/scan/reference/plm.md) solved
  [\#46](https://github.com/jazznbass/scan/issues/46): throws no error,
  when a phase is of length 1.
- [`corrected_tau()`](https://jazznbass.github.io/scan/reference/corrected_tau.md)
  solved [\#48](https://github.com/jazznbass/scan/issues/48): throws
  warning when A phase has less than three rows.
- solved [\#49](https://github.com/jazznbass/scan/issues/49): changes
  class from tibble to data.frame within scdf.

## scan 0.53

CRAN release: 2021-09-22

### Major changes

- scdf files now allow to combine studies with different phase designs.
  Several functions have been adapted to handle cases with differing
  designs in a mutual analysis.
- The `%>%` operator has been imported and exported from the magrittr
  package. Now that R 4.1 has a pipe operator, pipes seem to become the
  standard. For compatibility with older R Versions, we will stay with
  the `%>%` operator for some time before switching to `|>`.
- To allow for a piping code, we added several functions:
  `add_l2, select_phases, select_cases, subset, set_vars, set_dvar, set_mvar, set_pvar`.

#### New functions

- [`sample_names()`](https://jazznbass.github.io/scan/reference/sample_names.md):
  Returns a character vector of length `n` with names by randomly
  drawing from a name list: type = {“neutral”, “female”, “male”,
  “mixed”}. Useful to anonymize scdf files

``` r

names(exampleAB) <- sample_names(3)
```

\-[`add_l2()`](https://jazznbass.github.io/scan/reference/add_l2.md):
Adds the variables from a second level 2 data frame to an scdf matched
by an id variable (default is `case`).

``` r

Leidig2018 %>%
  add_l2(Leidig2018_l2) %>%
  hplm(update.fixed = .~. + gender + migration + ITRF_TOTAL*phaseB, 
       slope = FALSE, random.slopes = FALSE, lr.test = FALSE)
```

- [`select_phases()`](https://jazznbass.github.io/scan/reference/select_phases.md):
  selects and recombines phases into A and B phase (equivalent to the
  phases argument of various functions, but useful when using %\>%
  operators).

``` r

exampleA1B1A2B2 %>% 
  select_phases(A = c(1, 3), B = c(2, 4)) %>%
  overlap()
```

- [`set_vars()`](https://jazznbass.github.io/scan/reference/set_vars.md):
  change the core variables of an scdf (arguments `dvar` for dependent
  variable, `pvar` for phase variable, and `mvar` for measurement-time
  variable).

- [`set_dvar()`](https://jazznbass.github.io/scan/reference/set_vars.md),
  [`set_mvar()`](https://jazznbass.github.io/scan/reference/set_vars.md),
  [`set_pvar()`](https://jazznbass.github.io/scan/reference/set_vars.md):
  Shortcuts to set dvar, mvar, or pvar in a piping script
  e.g. `exmpleAB_add %>% set_dvar("depression") %>% describe()`

``` r

exampleAB_add %>%
  set_vars(dv = "depression") %>%
  overlap()
```

- [`is.scdf()`](https://jazznbass.github.io/scan/reference/is.scdf.md):
  Tests if an object is of type “scdf” or not.
- `check_scdf()`: Checks for the validity of an scdf object (mainly used
  for internal tests)
- [`convert()`](https://jazznbass.github.io/scan/reference/convert.md):
  Creates an scdf syntax file from an scdf object.

``` r

# Create a syntax to code the scdf exampleAB and write it into an R file
convert(exampleAB, file = "cases.R")
```

- `cdc`: Applies the Conservative Dual-Criterion Method (CDC; Fisher,
  Kelley, & Lomas, 2003) to scdf objects.

``` r

cdc(Beretvas2008)
cdc(exampleAB_decreasing, decreasing = TRUE, trend.method = "bisplit")
```

#### Changes in functions

- [`overlap()`](https://jazznbass.github.io/scan/reference/overlap.md):
  Added Hedges-g.
- new trend lines added to
  [`plot.scdf()`](https://jazznbass.github.io/scan/reference/plot.scdf.md):
  Koenig’s bi-split / quarter intersect (lines = “trendA_bisplit”) and
  Tukey’s tri-split / Wald’s slope (lines = “trendA_trisplit”).

``` r

plot(exampleAB_50[8], lines = "trendA_bisplit")
plot(example_A24, lines = "trendA_trisplit")
```

- [`plot.scdf()`](https://jazznbass.github.io/scan/reference/plot.scdf.md):
  Now allows for multiple lines with different line styles.

``` r

plot(
  exampleAB, 
  lines = list(
    list(type = "median", col = "red", lwd = 0.5),
    list(type = "trend", col = "blue", lty = "dashed", lwd = 2),
    list(type = "loreg", f = 0.2, col = "green", lty = "solid", lwd = 1)
  )
)
```

- [`tau_u()`](https://jazznbass.github.io/scan/reference/tau_u.md):
  Solved bug in meta analysis
  [\#6](https://github.com/jazznbass/scan/issues/6). Reworked the
  complete function to be more clear and accurate. Added `method_meta`
  switching between fixed and random-effect meta analyses. Reworked the
  print function to look nicer.

- [`export()`](https://jazznbass.github.io/scan/reference/export.md):
  Reworked the html output. Added a basic output for tau_u. Arguments
  `caption` and `footnote` allow to specify appearance (if left NA
  object specific output is generated.). `booktab = TRUE` is now set as
  a default for kable options.

#### Deleted deprecated functions

The following functions were deprecated since 2017 and are now removed
from scan:

- `makesingleSC()`
- `makeSCDF()`: Please use `scdf` instead.
- `estimateSC()`: Please use
  [`estimate_design()`](https://jazznbass.github.io/scan/reference/estimate_design.md)
- `power.testSC()`: Please use
  [`power_test()`](https://jazznbass.github.io/scan/reference/power_test.md)

#### Bugs

- [`print.scdf()`](https://jazznbass.github.io/scan/reference/print.scdf.md)
  now prints cases when all variable names are wider than the current
  screen with.

## scan 0.52

### Major changes

- [`describe()`](https://jazznbass.github.io/scan/reference/describe.md)
  as the new alias for `describeSC()`
- [`plot.scdf()`](https://jazznbass.github.io/scan/reference/plot.scdf.md),
  [`style_plot()`](https://jazznbass.github.io/scan/reference/style_plot.md):
  New options to style casenames: `names` which takes a list with tag =
  value structure. Example:

``` r

new_style <- style_plot()
new_style$names$side <- 3
new_style$names$line <- -1.7
new_style$names$col <- "darkred"
new_style$names$cex <- 1.5
new_style$names$at <- 20
new_style$names$adj <- 1
new_style$names$font <- 3
plot(exampleAB_decreasing, style = new_style)
```

- [`plot.scdf()`](https://jazznbass.github.io/scan/reference/plot.scdf.md),
  [`style_plot()`](https://jazznbass.github.io/scan/reference/style_plot.md):
  Different background colors for different phases:

``` r

new_style <- style_plot()
new_style$fill.bg <- c("aliceblue", "mistyrose1", "honeydew")
new_style$lty.seperators <- 0
plot(exampleABC, style = new_style)
```

``` r

plot(exampleABAB, style = c("default", "phase_shade"))
```

## scan 0.50.5

### Major changes

- fixed bug in plot function

## scan 0.50.4

### Major changes

- New [`subset()`](https://rdrr.io/r/base/subset.html) function (method
  from base [`subset()`](https://rdrr.io/r/base/subset.html) for
  selecting variables, rows, and cases. It takes the arguments `subset`,
  `select`, and `cases`.

``` r

subset(exampleAB, (values < 60 & phase == "A") | (values >= 60 & phase == "B"))
subset(exampleAB_add, select = c(-cigarrets, -depression))
subset(exampleA1B1A2B2, phase %in% c("A1", "B2"), cases = Pawel:Moritz)
```

## scan 0.50.2

### Major changes

- New
  [`select_cases()`](https://jazznbass.github.io/scan/reference/select_cases.md)
  function.

``` r

select_cases(exampleAB, "Johanna", "Karolina")
select_cases(exampleAB, 1,2)
select_cases(exampleAB, "-Johanna")
```

## scan 0.50

### Major changes

- Started dropping the `SC` extension from function names
  e.g. [`overlapSC()`](https://jazznbass.github.io/scan/reference/deprecated-functions.md)
  becomes
  [`overlap()`](https://jazznbass.github.io/scan/reference/overlap.md)

## scan 0.40

CRAN release: 2019-08-05

CRAN release 2019-08-11

## scan 0.20

CRAN release: 2016-10-14

CRAN release 2016-10-15

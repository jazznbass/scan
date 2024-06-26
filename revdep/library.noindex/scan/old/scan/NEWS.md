# scan 0.60.0

## New function

- `ird()`: Robust improvement rate difference as formulated by Postejovski (2019).

## Changes

- `pand()`: Rewrote function. New argument `method` allows to apply the sorting algorithm proposed in Parker 2007 and `method = "minimum"` applies the exact method provided by Pustejovski in 2019. Furthermore, the tau test was replaced with a X Squared and a Fisher exact test.
- `corrected_tau`: Report results with warning when all phase A data are identical; new argument tau_method = "a" to switch to Kendall's tau-a.
- `export()`: new for `pand()`.
- New example datasets: Tarlow2017, Parker2011b, Parker2009, Parker2007

## Shiny scan

- Added power-analyses
- Added settings
- Extended save options
- Various changes and optimizations of the ui

# scan 0.59.0

- `describe()`: now works correctly for duplicated phase names.
- shiny-app no longer depends on `shinyjs` and `markdown`. `shinyscan()` asks if missing packages `scplot` and `shiny` should be installed automatically. 

# scan 0.58

## Shiny app added

- start the app with `shinyscan()`
- you need the following packages to run the app:
  - `shiny`, `shinyjs`, `scplot`, `markdown`

## New fucntions

- `batch_apply()`: Apply a function to each element in an scdf. Use `.` as a placeholder for the scdf case.

```r
batch_apply(exampleAB, plm(.) |> coef())
```

- Helper functions for `transform()`: `n()`, `all_cases()`, `across_cases()`, `first_of`
- Helper functions for `transform()`: `moving_mean()`, `moving_median()`, `local_regression()`
- `corrected_tau()`: returns multiple cases
- `export()`: new for `nap(), pen()`
- `c()/combine()`: new arguments to set author and info attributes of the resulting scdf (`author`, `info`)

## Changes in functions

- `plm()`: Print function allows to set maximum lag for autocorrelations; Overall significance Ljung_Box test is reported.

```r
plm(exampleAB$Johanna) |> print(lag_max = 5)
```

## Bug fixes

- solved: `summary()` failed when one of the cases had no name
- solved: `nap()` only reported values for the first case with multiple cases.
- solved: `add_l2()` lost column name of l2 variable when l2 had only one variable.

## minor

- `convert()`: new arguments. indent sets the indentation. When the scdf contains only one case, no study is combined.
- `select_phases()`: New argument `phase_names` sets names of the recombined phases. The default `"auto"` creates combinations of the phase names automatically (e.g., `A = c("A", "B", B = "C")` results in phases `AB` and `C`).

# scan 0.56

## New features

- `export()`: New export for `power_test()` and `smd()` output.
- `export()`: `tau_u()` export with new argument `case` which takes the values `"meta"` or `"all"` and new argument `select` allowing to select, reorder, and rename specific variables.

- `select_cases()`: Allow for a selection based on object names (like in substitute).

```r
select_cases(exampleAB, -c(Johanna, Karolina))
select_cases(exampleAB, Johanna, Karolina)
v <- c("Moritz", "Jannis")
select_cases(exampleA1B1A2B2, v)
```

- `plm()`, `hplm()`: New arguments `contrast_level` and `contrast_slope` allow for setting the contrasts for level and slope separately. Both elements can either be "first" or "preceding".
- Speed-up `tau_u()` by 20%.
- rewrote `as_scdf()`, `read_scdf()`. Now it is easier to import data from any file format.

``` {.r}
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

## Bug fixes

- `tau_u()`: Rewrote the calculation of meta analyses and confidence intervals.
- `tau_u(), correted_tau()`: corrected a wrong calculation of the continuity correction when values where lower in phase B.
- `tau_u()`: Implemented a new method for calculating confidence intervals based on Fisher-Z transformations (see Long, J. D., & Cliff, N. (1997). Confidence intervals for Kendall’s tau. British Journal of Mathematical and Statistical Psychology, 50(1), 31-41.

## superseded function

- `smooth_caes()`, `shift()`, `standardise()`, `ranks()`, `truncate_phases()`: All superseded by `transform()` and its helper functions. See details in the help files of transform and in the scan-book.

## minor changes

- `as.data.frane.scdf()`/`as_scdf()`: keep and retrieve scdf attributes.

# scan 0.55

## new functions

- `coef()/ coefficients()`: Method for base R `coef` function for plm/hplm objects. Extracts coefficient tables from provided object.

```.r
coef(plm(exampleAB$Johanna))
```

## New features

- `power_test()`: New argument `ci` provides confidence intervals for power, alpha error, and correct proportions. New arguments `binom_test_alpha, binom_test_power, binom_test_correct` provide test against a provided proportion for alpha, power, and correct proportions.

```.r
design <- design(
  n = 1, phase_design = list(A = 6, B = 9),
  rtt = 0.8, level = 1.0, trend = 0.05
)

power_test(
  design, ci = 0.95, binom_test = TRUE
)
```

- `plm()`, `hplm()`: Added contrast argument with values `"first"` or "`preceding`". Now `model = "JW"` is deprecated and identical to `model = "B&L-B", contrast = "preceding"`.  
- `plm()`, `hplm()`: Added model `"W"`. Which shifts the measurement-time variable to start with zero. This leads to a more sensible estimation of the intercept (where the intercept depicts the estimated score at the start). `model = "W"` is now the default. Use `model = "B&L-B"` for previous defaults.

## Bug fixes

- solved #66: `set_vars()` working.
- `read_scdf()` now works when cvar is not the first column.

## Changes to functions

- `corrected_tau()`: changed default setting of arguments to: `repeated = FALSE, continuity = FALSE`. Now the default results match the calculator developed by Tarlow.

- `tau_u()`: Implemented a continuity_correction (S-1 for calculating Z)

# scan 0.54.1

## Bug fixes

- `select_phases()` now works when phase variable is not "phase"
- `estimate_design()` was broken and is fixed now.
- `print()` function for `sc_design`fixed.

# scan 0.54

## new functions

- `transform()`: Takes an scdf and calculates or modifies variables 
for each case (`transform(exampleAB, z_values = scale(values), t_values = 50 + z_values * 10)`).
- `smd()` reporting various types of standardized mean differences.

## reanmed functions (old functionnames still work)

- `readSC()` -> `read_scdf()` 
- `writeSC()` -> `write_scdf()`
- `design_rSC()` -> `design()`
- `rSC()` -> `random_scdf()`

## Complete rework - as new

- `power_test()` with various extensions, optimizations, and solved various bugs. rewrote the `print` method, added an argument `duration` to print the computation duration. Added the `'n_trials'` argument for binomial distributions. Extended the help page.  
- `design()` and its print method. Extended the help page. Rewrote the algorithm for the 'binomial' distribution.  

## Extended functions

- `plm()`: rewrote the analysis function for binomial tests. These now need an argument `var_trials` to define the number of trials per measurement. The  `dvar_percentage` argument must be set TRUE when the dependent variables are percentages (and `family = 'binomal'`).
- speed optimized `random_scdf()`. Rewrote the algorithm for 'poisson' distributed measures. Rewrote the algorithm for the 'binomial' distribution. Extended the help page.  
- `read_scdf()`: extracts filetype from file extension.
- `read_scdf()`: New `yaml` import options for scdf files

```yml
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

- `tau_u()` #51: Added option for confidence intervals for tau_u output.

## Solved error in functions

- `describe()`: solved wrong calculation of Hedges G when phase length differed.
- `plm()` solved #46: throws no error, when a phase is of length 1.
- `corrected_tau()` solved #48: throws warning when A phase has less than three rows.
- solved #49: changes class from tibble to data.frame within scdf.

# scan 0.53

## Major changes

- scdf files now allow to combine studies with different phase designs.
Several functions have been adapted to handle cases with differing designs in a mutual analysis.
- The %>% operator has been imported and exported from the magrittr package. Now that R 4.1 has a
pipe operator, pipes seem to become the standard. For compatibility with older R Versions, we will stay with the `%>%` operator for some time before switching to `|>`.
- To allow for a piping code, we added several functions: `add_l2, select_phases, select_cases, subset, set_vars, set_dvar, set_mvar, set_pvar`.

### New functions

- `sample_names()`: Returns a character vector of length `n` with names by randomly drawing from a name list: type = {"neutral", "female", "male", "mixed"}. Useful to anonymize scdf files

```R
names(exampleAB) <- sample_names(3)
```

-`add_l2()`: Adds the variables from a second level 2 data frame to an scdf matched by an id variable (default is `case`).

```R
Leidig2018 %>%
  add_l2(Leidig2018_l2) %>%
  hplm(update.fixed = .~. + gender + migration + ITRF_TOTAL*phaseB, 
       slope = FALSE, random.slopes = FALSE, lr.test = FALSE)
```

- `select_phases()`: selects and recombines phases into A and B phase (equivalent to the phases argument of various functions, but useful when using %>% operators).

```R
exampleA1B1A2B2 %>% 
  select_phases(A = c(1, 3), B = c(2, 4)) %>%
  overlap()
```

- `set_vars()`: change the core variables of an scdf (arguments `dvar` for dependent variable, `pvar` for phase variable, and `mvar` for measurement-time variable).

- `set_dvar()`, `set_mvar()`, `set_pvar()`: Shortcuts to set dvar, mvar, or pvar in a piping script e.g. `exmpleAB_add %>% set_dvar("depression") %>% describe()`

```R
exampleAB_add %>%
  set_vars(dv = "depression") %>%
  overlap()
```

- `is.scdf()`: Tests if an object is of type "scdf" or not.
- `check_scdf()`: Checks for the validity of an scdf object (mainly used for internal tests)
- `convert()`: Creates an scdf syntax file from an scdf object.

```R
# Create a syntax to code the scdf exampleAB and write it into an R file
convert(exampleAB, file = "cases.R")
```

- `cdc`: Applies the Conservative Dual-Criterion Method (CDC; Fisher, Kelley, & Lomas, 2003) to scdf objects.

```R
cdc(Beretvas2008)
cdc(exampleAB_decreasing, decreasing = TRUE, trend.method = "bisplit")
```

### Changes in functions

- `overlap()`: Added Hedges-g.
- new trend lines added to `plot.scdf()`: Koenig's bi-split / quarter intersect (lines = "trendA_bisplit") and Tukey's tri-split / Wald's slope (lines = "trendA_trisplit").

```R
plot(exampleAB_50[8], lines = "trendA_bisplit")
plot(example_A24, lines = "trendA_trisplit")
```

- `plot.scdf()`: Now allows for multiple lines with different line styles.

```R
plot(
  exampleAB, 
  lines = list(
    list(type = "median", col = "red", lwd = 0.5),
    list(type = "trend", col = "blue", lty = "dashed", lwd = 2),
    list(type = "loreg", f = 0.2, col = "green", lty = "solid", lwd = 1)
  )
)
```

- `tau_u()`: Solved bug in meta analysis #6. Reworked the complete function to be more clear and accurate. Added `method_meta` switching between fixed and random-effect meta analyses. Reworked the print function to look nicer.

- `export()`: Reworked the html output. Added a basic output for tau_u. Arguments `caption` and `footnote` allow to specify appearance (if left NA object specific output is generated.). `booktab = TRUE` is now set as a default for kable options.

### Deleted deprecated functions

The following functions were deprecated since 2017 and are now removed from scan:

- `makesingleSC()`
- `makeSCDF()`: Please use `scdf` instead.
- `estimateSC()`: Please use `estimate_design()`
- `power.testSC()`: Please use `power_test()`

### Bugs

- `print.scdf()` now prints cases when all variable names are wider than the current screen with.

# scan 0.52

## Major changes

-   `describe()` as the new alias for `describeSC()`
-   `plot.scdf()`, `style_plot()`: New options to style casenames: `names` which takes a list with tag = value structure. Example:

``` {.r}
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

-   `plot.scdf()`, `style_plot()`: Different background colors for different phases:

``` {.r}
new_style <- style_plot()
new_style$fill.bg <- c("aliceblue", "mistyrose1", "honeydew")
new_style$lty.seperators <- 0
plot(exampleABC, style = new_style)
```

``` {.r}
plot(exampleABAB, style = c("default", "phase_shade"))
```

# scan 0.50.5

## Major changes

-   fixed bug in plot function

# scan 0.50.4

## Major changes

-   New `subset()` function (method from base `subset()` for selecting variables, rows, and cases. It takes the arguments `subset`, `select`, and `cases`.

``` {.r}
subset(exampleAB, (values < 60 & phase == "A") | (values >= 60 & phase == "B"))
subset(exampleAB_add, select = c(-cigarrets, -depression))
subset(exampleA1B1A2B2, phase %in% c("A1", "B2"), cases = Pawel:Moritz)
```

# scan 0.50.2

## Major changes

-   New `select_cases()` function.

``` {.r}
select_cases(exampleAB, "Johanna", "Karolina")
select_cases(exampleAB, 1,2)
select_cases(exampleAB, "-Johanna")
```

# scan 0.50

## Major changes

-   Started dropping the `SC` extension from function names e.g. `overlapSC()` becomes `overlap()`

# scan 0.40

CRAN release 2019-08-11

# scan 0.20

CRAN release 2016-10-15


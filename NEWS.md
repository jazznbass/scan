

# scan 0.53.6

- rewrote `power_test()` with various extensions, optimizations, and solved various bugs. rewrote the `print` method, added an argument `duration` to print the computation duration. Added the `'n_trials'` argument for binomial distributions. Extended the help page.  
- speed optimized `rSC()`. Rewrote the algorithm for 'poisson' distributed measures. Rewrote the algorithm for the 'binomial' distribution. Extended the help page.  
- rewrote `design_rSC()` and its print method. Extended the help page. Rewrote the algorithm for the 'binomial' distribution.  
- new function `mc_function()` takes a character string and returns a function for Monte Carlo analysis.  
- `plm()`: rewrote the analysis function for binomial tests. These now need an argument `var_trials` to define the number of trials per measurement. The  `dvar_percentage` argument must be set TRUE when the dependent variables are percentages (and `family = 'binomal'`).


# scan 0.53.5

- solved wrong calculation of Hedges G when phase length differed.
- new function `smd()` reporting various types of standardized mean differences.

# scan 0.53.4

- `load_scdf` and `save_scdf` replace `readSC` and `writeSC`. The latter are kept working.
- `load_scdf` extracts filetype from file extension.
- New `yaml` import options for scdf files

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

# scan 0.53.3

- solved #51: Added option of CI for tau_u output.

# scan 0.53.2

- solved #46: `plm` throws no error, when a phase is of length 1.
- solved #48: throws warning for `corrected_tau` when A phase has less than three rows.
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

- `select_phases()`: selects and recombines phases into A and B phase (equivalent to th phases argument for various functions, but useful when using %>% operators).

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

- `subset()`: Argument `subset` changed to `filter`.
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

see: <https://jazznbass.github.io/scan-Book/the-scan-package.html#changes-with-version-0.50>

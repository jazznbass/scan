# scan 0.69.0 release candidate

## Breaking changes

- Tables are built with the gt package by default, where kableExtra was used before. gt renders markdown and html in captions and footnotes, carries the column and row groups of every export method, and writes Word documents. A document knitted to latex still uses kableExtra, because gt tables receive neither a number nor a label there; Word output always uses gt, with a message, because kableExtra can not write usable Word tables. `options(scan.export.engine = "kable")` restores the previous behaviour for html output.
- `estimate_design()` returns different reliabilities. `rtt` is now estimated as the quantity `random_scdf()` generates data from, so designs estimated with an earlier version of scan, and data simulated from them, are not reproduced.
- `estimate_design()` warns and falls back when `s` can not be estimated. For cases that do not differ at baseline — a single case, two cases, data from `design(random_start_value = FALSE)` — the between case variance consists of estimation error only. `s` is then set equal to the standard deviation of the error, which fixes the reliability at 0.5 by construction and expresses the effects in units of the variation within a case. Three warnings say so; giving `s`, `rtt` or `error` is recommended for such data.
- `plm()` models autocorrelation along the measurement-time variable, as `hplm()` already did. Results change for cases whose measurement times are not consecutive, where the gaps were previously ignored.
- `moving_mean()` and `moving_median()` average the original series. The loop wrote its results into the vector it was reading from, so from the second value on, already smoothed values entered the window: the filter pulled the series in the direction the loop ran and would have given other numbers running backwards. Smoothed series therefore differ from earlier versions of scan — in `transform()`, in the smoothing lines of `plot()` and in the deprecated `smooth_cases()`. The first and last `lag` values are left unchanged as before.
- `scdf()` rejects a phase design that is defined in more than one way, instead of letting one definition win.
- Regression models across several cases reject cases with differing phase designs and name the case, instead of failing later.
- Selecting an unknown case with `$` or `[` raises an error instead of returning an scdf whose case is `NULL`.
- Removed the function names that were replaced when the `SC` suffix was dropped. They had been forwarding to their successor with a warning since scan 0.50 and 0.54: `tauUSC()` → `tau_u()`, `power_testSC()` → `power_test()`, `fillmissingSC()` → `fill_missing()`, `overlapSC()` → `overlap()`, `randSC()` and `rand.test()` → `rand_test()`, `rciSC()` → `rci()`, `rSC()` → `random_scdf()`, `readSC()` and `readSC.excel()` → `read_scdf()`, `writeSC()` → `write_scdf()`. The functions deprecated in 0.58 — `smooth_cases()`, `shift()`, `standardize()`, `ranks()`, `truncate_phase()` — are unaffected and still work.
- `smd()`: removed the `mvar` argument, which had no effect.
- `cdc()`: the result fields `cdc_be` and `cdc_b` are now named `cdc_exc` and `cdc_nb`, as the help page has always documented.
- `pand(method = "minimum")`: the result field `perc_overlaps` is now named `perc_overlap`.

## New features

- `design(error = ...)` and `estimate_design(error = ...)` set the standard deviation of the measurement error directly, as an alternative to `rtt`, from which the reliability is derived as `s^2 / (s^2 + error^2)`. `error` is the quantity a study reports as residual variation; for a single case, or for cases that hardly differ at baseline, it is the parameter that can be pinned down. Both arguments also accept one value per case.
- `fill_missing(mark = TRUE)` adds a logical variable `interpolated` flagging every measurement that contains an interpolated value.
- `autocorr(na.action = ...)` allows autocorrelations from incomplete series.
- `plm()`, `hplm()`, `mplm()` and `bplm()` record the names of their level and slope dummy variables, so covariates such as `intervention` or `phase_length` are no longer renamed as if they were dummies.

## Bug fixes

### Wrong results

- `read_scdf()` rejects file types it can not read. A type matching none of the import branches left the internal data object unassigned, so R resolved that name in the user's workspace and, with an object of that name present, returned it as the file's content without any warning. `type` is matched case-insensitively now, so `type = "CSV"` works.
- `convert(inline = TRUE)` writes one section per phase of the series. The measurements were grouped by the levels of the phase variable, so in every reversal design all sections sharing a name were merged and the measurements came back in a different order — valid code producing the wrong data.
- `convert()` quotes the phase names in the generated code, keeps the position of the measurement-time variable, and no longer writes a trailing comma. Phase names with a space, a hyphen, a number or a reserved word produced code that did not parse, data whose measurement times are not the last variable came back with reordered columns, and a case with default variable names produced `scdf(..., )`.
- `outlier()` treats each phase as the section of the series it occupies, not as everything carrying the same label. With repeated labels — an ABAB design — the filter marking the outliers became twice as long as the case: the returned data held `NA` rows, the wrong measurements were removed and `dropped.n` counted outliers twice. Affected `method = "SD"`, `"MAD"` and `"CI"`.
- `outlier()` ignores missing values when the bounds are computed and matches Cook's distances to the measurements they belong to. A single missing value made both bounds of its phase `NA` and replaced the whole phase by `NA` rows; for `"Cook"` the shorter filter was recycled over the case, removing arbitrary measurements.
- `rand_test()` reports `NA` with a message when the observed statistic or the randomization distribution is not finite, instead of building a p value from such values. `Inf >= Inf` counted as a hit, so a constant baseline with `statistic = "SMD glass"` returned p = 0.55 from six infinite values out of eleven. `Z` and `p.Z.single` are `NA` instead of `NaN` when the distribution has no variance, and `statistic = "T-test"` no longer stops inside `t.test()` on a constant split.
- `rand_test(statistic = "Slope A-B")` computes the difference in the direction it names. It passed the method of `"Slope B-A"` on, so for a rising series it reported p = 0.00 where p = 1.00 is correct.
- `rand_test()` checks `limit` against the length of the series, assigns one set of `startpoints` per case as documented, and draws correctly when only one start point is admissible. Phase A could fall below the minimum unnoticed, a list of start points was handed to every case, and `sample()` treated a single admissible point as a range.
- `estimate_design()` estimates the reliability as `s^2 / (s^2 + error variance)`, the quantity `random_scdf()` uses under that name, instead of the R squared of the piecewise regression. The old estimate followed the size of the effect: a case without any effect came back with a reliability near zero, which `random_scdf()` turned into an exploding error term. The error variance now uses the degrees of freedom of the model, and the uncertainty of the estimated start values is subtracted from the between case variance. Simulating with 0.50, 0.70, 0.80 or 0.95 and estimating it back returns 0.51, 0.71, 0.80 and 0.95.
- `estimate_design()` keeps a reliability passed to it. `overall_rtt` is documented as being ignored when `rtt` is set, but the estimate overwrote the given value in every case — including in the example on the help page.
- `design()` hands the whole `extreme_range` to every case instead of one bound per case, and rejects a range whose first value is not below the second. `random_scdf()` had drawn against an undefined bound and written missing values into the simulated data — seven of forty measurements with the default settings, twenty of forty for a reversed range.
- `random_scdf()` keeps count data as counts when extreme values are added. Binomial and Poisson data came back with decimal places, and binomial counts could exceed the number of trials, which gave `plm(family = "binomial")` proportions above 1.
- `random_scdf(3)` builds the three cases it was asked for; the number was noted and then overwritten, so a single case was returned.
- `select_cases()` applies every selection to the same object instead of one after the other. `select_cases(exampleAB, -Johanna, -Karolina)` returned four cases — both excluded ones among them, one of them twice. Mixing a positive and a negative selection is now an error, and the arguments are resolved in the calling environment, so names held in a variable are found inside a function.
- `combine()` and `c()` make duplicated case names unique and warn. Cases sharing a name could not be told apart: `add_l2()` gave both the same level-2 row, `select_cases()` and `x$name` returned only the first.
- `batch_apply(simplify = TRUE)` counts the rows per case before combining them, so each row carries the case it came from. Where the cases contributed unequally many rows, the labels were wrong or the call failed. The expression is evaluated in the calling environment, and the column names are no longer mangled by `check.names`.
- `hplm(lr.test = TRUE)` builds its likelihood ratio tests from the estimated random effects instead of the text of the formula. Names containing a `1` were mangled — `phaseB1` became `phaseB - 1` — and a formula with an implicit intercept produced fewer tests than there were random effects.
- `mplm()` fits the null model to the rows the full model used, and addresses the response by the names of the dependent variables. With missing values the two models used different data, so `anova()` and `print()` failed; a user supplied `formula` failed with `variable lengths differ`.
- `plm()` computes the F test and R squared from the same residual sum of squares. R squared used `var(residuals)`, which is only equivalent for ordinary least squares. Results for `AR = 0` are unchanged.
- `bplm()` keeps a random effects formula passed through `random`, which was replaced whenever `random_trend`, `random_level` or `random_slope` was set.
- `hplm()` reports `random.slopes` correctly when the random effects were requested through `random_trend`, `random_level` or `random_slope`.
- `tau_u()` returns `NA` with a warning for cases with fewer than two observed values in a phase, where it produced a plausible looking but invalid Tau-U; the meta analysis follows. The zero variance check of the internal Kendall tau tested the first variable twice and never the second.
- `trend()` raises an error for custom models with more than one predictor, where it silently reported the second raw coefficient in the `Beta` column.
- `pand(method = "sort")`: the `decreasing` argument no longer reverses the phase tiebreak, so both directions are treated symmetrically.
- `pem()` counts the exceeding measurements once and uses that count for the percentage, the binomial test and the chi-squared test, which previously reconstructed it from the percentage.
- `describe()` handles phases without observed values, which returned `Inf`, `-Inf` and `NaN` and stopped the whole call inside `lm.fit()`. The trend no longer depends on the global `na.action` setting.
- `fill_missing()` interpolates the missing values of the measured variables again — only absent measurement times had been filled — determines measurement times missing at the beginning or the end of a series, and preserves supplied measurement times instead of rounding them. A case whose measurement times remain unknown is returned unchanged with a warning; such observations were previously pushed to the end and replaced by interpolated values.
- `power_test()` tests the falling direction with `rand_slope_decrease`, which used the same statistic as `rand_slope`, and uses the requested `ci` level for all three confidence intervals, where the interval for the correct proportion was always a 95 % one.
- `add_l2()` gives a case without a matching row in the level-2 data `NA` instead of leaving out the level-2 variables, which made the scdf unusable; `hplm()` warns when cases are dropped from the model because of missing values.
- `trend()` and the trend difference of `overlap()` remove missing values themselves instead of following the global `na.action` setting. With `na.action` set to `"na.pass"` — which any package may leave behind, for instance a sampler that aborted — the missing values reached the regression and the call stopped with `NA/NaN/Inf in 'y'`, a message pointing at nothing recognisable. `describe()`, `cdc()`, `pet()` and `mplm()` already set it explicitly; `trend()` now also works with incomplete series.
- `autocorr()` reports missing values with a message pointing to `fill_missing()` instead of failing inside `acf()`, and handles phases with fewer than two observations.
- The phase selection is applied before missing values are removed in `pnd()`, `pem()`, `nap()`, `pand()`, `ird()`, `corrected_tau()`, `cdc()` and `rand_test()`; cases without observed values in a selected phase are rejected or skipped. Empty phases in PEM and NAP, unusable cases in PAND and IRD with correct case counts, and insufficient data in `corrected_tau()`, `cdc()`, `pet()` and `rci()` are handled; CDC overall results stay missing when any case is unevaluable.
- Corrected further data-preparation functions: phase definitions and `phase_starts` with repeated measurement times in `scdf()`, case naming in `combine()`, custom level-2 IDs and column overwrites in `as.data.frame.scdf()`, single remaining variables and created case variables in `as_scdf()`, logical row filters and empty selections in `subset.scdf()`, long expressions and caller-local variables in `transform()`, the centering position in `center_at()`, out-of-range replacements in `set_na_at()`, series shorter than the window in `moving_mean()` and `moving_median()`, and automatic phase naming in `select_phases()`.
- The `mad` column of `outlier(method = "MAD")` reports the scaled median absolute deviation the bounds are built from; it used `constant = 1`, so bounds read off the matrix were too narrow by a factor of 1.4826. The bounds themselves are unchanged.

### Calls that stopped

- Argument checks accept vectors where the function documents them. The range check used `&&`, which requires a single value since R 4.3, so `design(extreme_prop = c(0.1, 0.3, 0.5))` stopped with a message naming neither the argument nor the function.
- Functions that validate their arguments can be called programmatically again. The check looked the function up by the name it was called under, so `do.call(plm, args)` and `lapply(list_of_scdf, plm)` failed. Affected `plm()`, `hplm()`, `mplm()`, `bplm()`, `pand()`, `tau_u()`, `cdc()`, `corrected_tau()`, `between_smd()`, `rand_test()`, `design()` and `add_dummy_variables()`.
- `design(B_start = ...)` works without `mt`, which defaulted to `NULL` although the help page documents 20, so the very call the help page describes stopped with `replacement has length zero`.
- `design()` rejects phase lengths that are not whole numbers of at least one, which were accepted and only failed later inside `random_scdf()` with `invalid 'times' argument`.
- `estimate_design()` no longer builds a design object out of an undefined `s`, which made `random_scdf()` produce a case of nothing but missing values. An `s` that is zero, not finite or not a number is rejected.
- `set_vars()`, `set_dvar()`, `set_mvar()` and `set_pvar()` reject a variable that is not part of every case; a typo surfaced only in the next analysis, and in the overlap indices not at all.
- `rescale()` takes its variable names as characters or from a variable as well as as object names, and rejects a name it cannot use. `rescale(dat, "values")` looked for a column whose name carries the quotation marks.
- `sample_names()` and `random_scdf(random_names = ...)` reject a type they do not know; `"Male"` or any typo returned an empty character vector without a word.
- `fetch()` reports an unsupported `what` instead of returning an invisible `NULL`.
- `write_scdf()` writes to the console when `filename` is `NULL`, its documented default, which stopped with `argument is of length zero`.
- `combine()` without any argument says what is missing instead of failing with `subscript out of bounds`; the same for `select_cases()` without a selection.
- `tau_u(ci = NULL)` works, as documented; `ci` is validated and accepts `NULL`, `NA` or a value between 0 and 1.
- `anova()` for `hplm()` objects passes additional arguments on unchanged; the call was assembled as text, so `anova(model, type = "marginal")` failed with `object 'marginal' not found`.
- `plm()` passes `...` on to `nlme::gls()` when `AR > 0`, where the arguments were dropped without notice.
- `plm()` and `hplm()` check the measurement times before modelling autocorrelation, which requires whole numbers that are unique within a case, and report the problem instead of stopping inside `corARMA()` or ignoring the times.
- `power_test()` accepts unnamed functions in `method`, no longer stops when a binomial test is requested while `alpha_test` or `power_test` is switched off, and reports an undefined alpha to beta ratio as missing instead of `1:NA`, `1:Inf` or `1:NaN`. Its print method shows the p value for the correct proportion when it was asked for; the block was guarded by `binom_test_power`, so `binom_test_correct` on its own computed the value but never printed it.
- `power_test()` reports the p values of its binomial tests with three decimals; `p_power` and `p_alpha` were rounded to whole numbers and could only be 0 or 1, which suggests the opposite of what the test says.
- `export()` accepts a footnote of several lines for every kind of object; `if (is.na(footnote))` stopped the call with `the condition has length > 1` for the vectors the package's own default footnotes have.
- `export()` for `plm()` derives the column groups from the table instead of assuming its shape. Of the 108 combinations of family, `ci`, `q`, `r_squared` and engine, 70 either failed or produced a mislabelled table — `ci = FALSE` labelled `SE` and `t` as confidence limits, `ci = TRUE` read `CI(100%)`, and every binomial export failed under kable.
- `export()` for a `tau_u()` result works for a single case under the kable engine, where the rows were combined by a loop starting at the second table.
- `export()` for `pand(method = "minimum")` no longer accesses statistics that only exist for `method = "sort"`, and `export()` for `nap()` formats the p values before the columns are selected, so a selection without `p` works.
- `print()` for a `design()` object works when the cases differ in their proportion of extreme values, which could be created but not looked at.
- `print()` for an scdf works with `cols = "main"` and with a single column. The variable names were read with `attr()` instead of from the `scdf` attribute, so the documented `options(scan.print.cols = "main")` broke every scdf print.
- `print()` for `plm()` objects works with `ci = FALSE`, which stopped with `object 'param_filter' not found`.
- `print()` and `export()` for a `rand_test()` result survive a degenerate randomization distribution: a p value of `NA` stopped the output, and a distribution without variance stopped it inside `shapiro.test()`. The normality test is computed from the finite values and skipped with a note when there are too few.

### Printed and exported output

- `export()` for a `rand_test()` result returns a table of the statistics instead of the console output, which was packed into a single cell as html and only worked with gt in html. The cases, the phases compared, the basis of the distribution and the direction of the p value are in the footnote.
- `print()` for an `outlier()` result reports the criterion again, and `export()` returns a table instead of the captured console output. The result object did not carry the method, so none of the four branches of the print method ever applied: neither the criterion nor the matrix of bounds was shown, not even in the help page examples.
- The export methods no longer branch on the table engine. Horizontal rules and bold columns were added by the method itself, for the kable engine only, so the gt tables of `hplm()` and `bplm()` had no rule between the fixed and the random effects and the `pand()` tables no bold labels. `export()` for `hplm(casewise = TRUE)` even built its table twice, once per engine, so the gt version bypassed the central builder and with it the blanking of non-finite values, the decimals and the latex handling. The methods now describe what they want — row groups, column groups, rules, bold columns — and the builder alone knows the engine. The `pand()` table also lost a doubled column group under kable, which the method added a second time after the builder had already placed it.
- Row and column groups reach the table in both engines. Both were described twice — once per engine — and had drifted apart: headings were commented out, groups reached over the wrong rows or were added twice, and under kable the groups of `corrected_tau()`, `autocorr()`, `between_smd()`, `hplm()` and `bplm()` were missing or doubled. For `corrected_tau()` and `autocorr()` the case a block of rows belongs to is now named at all. The `tau_u()` table no longer has a separate kable layout with a `Case` column and empty separator rows.
- Latex tables spell out the characters pdflatex does not know and drop markup it can not render: the greek letters of `pand()` and the superscript two of `plm()` and `nap()` stopped the compilation and now read `Phi`, `Chi-squared`, `R-squared`. The decision follows the format of the table, so console and html output keep `R²` and render their tags — it previously followed `knitr::is_html_output()`, which is `FALSE` whenever no document is being knitted, so plain R scripts got latex tables under kable.
- `export()` leaves a cell empty where a value could not be computed. gt wrote `NaN` where kable left a blank; both now show an empty cell, infinite values included. In a `rand_test()` table, the summary of the distribution and a p value of `NA` are blanked the same way instead of reaching the reader as the text `Inf` or `NA`.
- The `decimals` argument of `export()` has the same effect in both engines. It reached only gt, so the same table was printed with different precision depending on the engine; `export()` for `mplm()` passes the argument on at all.
- The p values of the parametric tests are formatted the way the coefficient tables already were. The Pillai trace of `mplm()`, the likelihood ratio test in `hplm()`, the chi squared and Fisher tests of `pand()`, the Shapiro-Wilk test on the randomization distribution, and the F test, chi squared test and Ljung-Box test of `plm()` were written with three decimals, so a clear result read `p = 0.000`. The p value of `rand_test()` itself is deliberately left as it is: it is an observed proportion whose resolution is one divided by the number of permutations.
- `summary()` of an scdf returns the summary instead of writing it, so `export(summary(scdf))`, an assignment or a call inside another function no longer print the console text. A `print()` method for the `scdf_summary` object writes it, and `all_cases` can be given to either.
- `export()` with the kable engine no longer escapes html in footnotes, which showed tags as `&lt;b&gt;` — the package's own footnote for `summary()` of an scdf among them — and adds its footnote to latex tables as well, where the table itself was lost because `kable_styling()` marks only html tables with the class the footnote step looked for. Latex output keeps escaping, so a percent sign still reaches the pdf intact.
- `export()` for `summary()` of an scdf builds its footnote without html, which reached pdf output as a stray `..gt_linebreak_indicator..` under gt and as visible tags under kable.
- `export()` for `hplm()` and `bplm()` uses a footnote passed through `footnote`, which was replaced by the generated one without notice; `export()` for `smd()` likewise overwrote it with its six explanatory lines.
- `export()` for a `rand_test()` result carries a footnote naming the number of cases and, where they are named, the case names; the block meant to build it was empty.
- `export(select = ...)` keeps the table a table when a single column is selected, names the columns it can not find, and keeps the original names when a selection is given as numbers with names, such as `c(Case = 1, 3)`. Column numbers outside the table are rejected by name.
- `print()` for a `pand(method = "sort")` result computes the totals of its two by two matrices by row, where both matrices carried the column sums in their `total` column. The exported table was already right, so print and export contradicted each other.
- `print()` and `export()` report the AIC of a `plm()` model with `AR > 0`, which showed as `NA` because the value was taken from a list element only `glm` objects carry, and the printed AIC is rounded.
- `print()` for `hplm()`, `cdc()`, `mplm()` and `plm()` reports which variables the analysis used when they are not the default ones, as the other print methods do. For `mplm()` the line existed but was commented out.
- The note on the variables used no longer fails when an object does not carry all three variable attributes, and reports only the attributes that are set.
- Long messages and warnings are truncated at a word boundary instead of in the middle of a word.
- `shinyscan()` restores every option the app touched when it closes. `old_opt` was overwritten by the second `options()` call, so passing an scdf to the app left `shiny.launch.browser` set afterwards, and `scan.shiny.theme` was never saved at all. The export options the app sets for itself — engine, table styling, title prefix — are now saved and restored as well, instead of staying in the user's session after the app is closed.
- The export methods restore only the options they set themselves. `export()` for an scdf, for its summary and for a `tau_u()` result saved the complete set of options and wrote it back, undoing changes made elsewhere in the meantime; `export()` for `pand()` did not restore `knitr.kable.NA` at all when the call failed.

## Documentation

- `scdf()`: the help page described a priority order for competing phase-design definitions that did not match the behaviour.
- `overlap()`: PAND is reported for `method = "sort"` while IRD is based on `method = "minimum"`, so the two columns are not algebraically linked; removed the `design` entry from the return value, which was never returned.
- `outlier()`: MAD is the median absolute deviation, not the "mean average deviation", and `criteria` refers to the scaled deviation returned by `stats::mad()`.
- `trend()`: the `model` argument no longer lists `phase`, which cannot be estimated within a single phase.
- `cdc()`: removed the `phases` entry from the return value, which was never returned.
- `rescale()`: the `...` argument documents that the variables can be named as objects or as characters.
- `hplm()`: `data.l2` requires a column named `case`, not `cases`; the likelihood ratio test for the ICC tests a variance against zero, so its p value is conservative.
- `mplm()`: the `formula` argument states that the response is the `cbind()` of the dependent variables.
- `bplm()`: the return value `mcmglmm` is named `mcmcglmm`, the description of `formula` referred to the hplm model, and an example announced a random slope while setting `random_level`.
- `anova()`: the example for Poisson models compared them with a Gaussian model fitted to different data.
- `plm()` and `hplm()`: the `AR` and `ar` arguments state that the measurement times must be whole numbers and unique within a case.
- `plm()`: the return value describes `contrast`, `var_trials`, `dvar_percentage` and `data`.
- `power_test()`: the return value names the class `sc_power` instead of describing the result as a data frame.

## Internal changes

These do not change what the package does, but they removed a trap or a duplicate.

- The row groups and column groups of a table are described once and translated by the table builder, instead of being written out separately for each engine. That duplication is what the group bugs above came from.
- The function behind the two slope statistics of `rand_test()` is registered under the name the lookup uses. It had been stored under a different one and only worked because the same line left an object of a matching name in the namespace, which `match.fun()` picked up; removing that stray assignment would have silently disabled both statistics.
- `export()` for `mplm()` no longer relies on lazy evaluation to survive the kable engine: the column group was created only in the gt branch but handed over in every case.
- The checks written while fixing these bugs became regular tests. New test files cover `outlier()`, `export()` — engine resolver, latex and Word fallback, `select`, footnotes, decimals, non-finite cells, and the structural parity of the two engines across all export methods — the print methods, `random_scdf()`, `select_cases()`, `set_vars()`, `rescale()`, `combine()` and `sample_names()`; the tests for `design()`, `estimate_design()` and `rand_test()` were extended. Simulation-heavy blocks are skipped on CRAN.

# scan 0.68.1

- Introduced rlang error and messaging system for more informative error messages and warnings.

# scan 0.68.0

## New features

- Reworked `fill_missing()` function. It now interpolates the values for all variables in an scdf.

## Shinyscan app

- Introduced tabset in the data load and example data tab.
- Tabsets in the plot dataset with many added options.
- Tabsets in the stats tab with Arguments on a separate tab.

## Corrections

- Extended roxygen helpfiles for many functions.

## Fix

- export function for `plm()` does not throw an error when AIC can not be computed.

## New example datasets

- example_stranger: example for screen time of Stranger Things characters.
- example_atd: three case example for AB alternating treatment design.

# scan 0.67.0

## Reworked Shiny Scan app

- Complete new design for the "plot" tab.

## New features

- `shinyscan()`: New argument theme (default is `cerulean`) that allows to run shinyscan in a different bootstrap 5 theme.
  I like `shinyscan(theme = "united")`
- `export()` methods for: `cdc()`, `corrected_tau()`, `rand_test()`, `ird()`, `outlier()`, `autocorr()`.

# scan 0.66.0

## New features

- `as_scdf()`: Add format checks with informative error messages.  
  (Mainly useful for externally loaded files via `read_scdf()`.)
- `shinyscan()`: New argument `browser`. Default `"external"` launches the Shiny app in an external browser.
- `between_smd()`:  
  - Stops if the `scdf` contains only one case.  
  - Redesigned outputs.  
  - Improved Bayesian analysis.  
  - Added confidence/credible intervals and new argument `ci`.
- `hplm()`: 
  - Argument `ar` sets a auto-regression parameter for correlated residuals.
  - Argument `unequal_variances` weights the estimations by within phase variances.

## New shiny app for importing data

- `import_scdf()`: opens a small import menu. Also available as an addin in R Studio.

## Reworked Shiny Scan app

- Redesign the “Add case” workflow.
- Added an import procedure to choose variable names at import.
- Now it is possible to start shinyscan with an scdf object and directly start to analyse it `shinyscan(exampleABC)`.
- Stats tab: auto-fill the “Output arguments” field when a statistics function is selected.
- Stats tab/ Settings tab: "Description" switch in settings can be set to provide additional short descriptions for each stats function.
- Switch to Bootstrap 5.
- Miscellaneous visual polish.

## Error correction

- Addin in R-Studio is now renames to "Lauch Shiny-Scan" and finally works.

# scan 0.65.1

## Error correction

- `plm()`: dummy slope values did falsely ignore missing values. That could lead to incorrect slope effect estimations.

# scan 0.65.0

## New function

- `rowwise()`: A new helper function for `transform()` that allows to make calculations rowwise:

```.r
ex <- exampleAB_add; ex[[1]]$wellbeing[c(3, 6)] <- NA
transform(
  ex, 
  mean_dv = rowwise(mean(c(wellbeing, cigarrets, depression), na.rm = TRUE))
)
```

## New features

- `mplm()`: Reworked the function and its output. Now provides global F test and a more integrated depiction of the coefficients. Added export() method.
- `plm()/print.sc_plm()`: either print partial or delta (incremental), or both R squared: 

```r
plm(exampleAB$Johanna) |> 
  print(r_squared = c("delta", "partial"))
```

## Corrections

- `plm()`: Corrected calculation for model fit F statistic and R2 for models without an intercept
- `scdf()`: Reported a false error message when the `phase.start` argument was used and mt started with 0.

## Further changes

- `scdf()`: Optimized code, function arguments, and help page.

# scan 0.64.0

## New functions

- `fetch()`: General getter function to extract components from a scan object. It takes a scan object and and an optional argument and returns sub-objects. 
For now, it extracts the regression object of class glm, lm, lme from the respective plm, hplm, and mplm objects:

```r
mod <- plm(exampleAB$Johanna)
fetch(mod)
```

## New features

- `anova()`: Implemented functionality of further arguments of the generic anova functions.
- `options(scan.string.dummy.phase = "phase")`: can be renamed to avoid name conflicts in the output of regression models.
- `options(scan.string.dummy.slope = "inter")`: can be renamed to avoid name conflicts in the output of regression models.

# scan 0.63.0

## New functions

- `bplm()`: Bayesian piecewise regression model. Applies a Markov Chain Monte Carlo sampler from the MCMCglmm package. With export method.
- `add_dummy_variables()`: Helper function that adds dummy variables necessary to calculate a plm to an scdf. 
- `anova.plm() anova.hplm()`: Methods for likelihood ratio model comparison.

## New features

- `hplm()`: Adds inter correlation of random variables to the print output.
- new option: `scan.rename.predictors` can be set to `no`, `concise`, or `full`. Changes how predictors of regression models are renamed.
- `between_smd()`: Added support for Bayesian regressions `model = "bayesian"` or providing an object returned from the `bplm()` function.

## Solved bugs

- `plm()`: setting `var_trials` to a constant threw an error.

## Corrections / Changes

- `between_smd()`: Reworked the function output to avoid confusion. A 'pure' between case smd estimation is provided when the argument `include_residuals = FALSE` is set.
- `print.sc_plm() / export.sc_plm()`: New argument `ci` for specifying a confidence interval. Either `FALSE`, `TRUE` or a number between 0 and 1 (0.90 for a 90% intervals).

# scan 0.62.0

## New functions / features

- `rescale()`: New function as a helper for getting standardized estimators in regression models. e.g. `exampleAB |> rescale() |> hplm()`.
- `between_smd()`: Calculates between case standardized mean differences as proposed by Pustejovsky et. aL (2014). Can take complex hplm models as a basis.
- `na.omit.scdf()`: scdf method for generic `na.omit()`. Removes any row with a missing value from an scdf.

## New features

- `design()`: Argument `random_start_values` randomly assigns start values for each case based on the distribution (`normal`, `poisson` or `binomial`) and the respective parameters (`start_values`, `s`, `n_trials`).
- `print.sc_hplm()`: New argument `smd`. If set TRUE, between case smd results are reported.
- `tau_u()`: New method `"tarlow"` calculates Tau-U as implemented in an R code and online calculator by Tarlow (2017). Here, tau values are calculated as in the `method = "complete", continuity_correction = TRUE, tau_method = "a"`. Inferential statistics are calculated based on tau b and the standard deviation for S is derived directly from Kendall's Tau B analysis (different from the `parker` and `complete` methods). 
- `rand_test()`: It is now possible to provide new functions for calculating the statistic directly with a list to the `statistic_function` argument. This list must have an element named `statistic` with a function that takes two arguments `a` and `b` and returns a single numeric value. A second element of the list is named `aggregate` which takes a function with one numeric argument that returns a numeric argument. This function is used to aggregate the values of a multiple case design. If you do not provide this element, it uses the default `function(x) sum(x)/length(x)`. The third optional argument is `name` which provides a name for your user function.

```r
userstat <- list(
  statistic = function(a, b) median(b) - median(a), 
  aggregate = function(x) median(x),
  name = "median B - A"
)

rand_test(exampleAB, statistic_function = userstat , complete = TRUE)

# which is identical to:
rand_test(exampleAB, statistic = "Median B-A" , complete = TRUE)
```

- `rand_test()`: Returns startpoints for the random permutations.
- `plot_rand()`: New argument `type` when `"xy"` a plot with splitpoints and statistics is drawn. This allows to see graphically at which measurement time a statistic changes.

```r
Leidig2018[4] |> 
  na.omit() |> 
  rand_test(complete = TRUE, limit = 1, statistic = "SMD glass") |> 
  plot_rand(type = "xy")
```


## Corrections / Changes 

- `scdf()`: Throws an error when argument `phase_starts` is set and the beginning of the first phase is not the first measurement.
- `tau_u()`: Method `"parker"` ignores the `tau_method` setting and sets `continuity_correction = FALSE`. This follows the Parker (2011) paper. There, the inferential statistics are calculated using Kendall's Tau b while the actual Tau calculation applies Kendall's Tau a (without ties).
- `rand_test()`: Missing values in the dependent variable are now removed before calculations.

## Solved bugs

# scan 0.61.0

## Solved bugs

- Corrected the name of the level-effect predictor for regressions when the phase variable is not named "phase".

## New

- Tip-of-the-day like message at start-up.
- Multiple improvements of the Shiny app (try out with `shinyscan()`)
- new output engine for rendering html export based on *gt table*. Set `options(scan.export.engine = "gt")`. This engine allows to export tables into docx format: `overlap(exampleAB) |> export(file = "test.docx", flip = TRUE)`.
- new export functions for `pem()`, `pet()`, `pnd()`, and `summary()` (either `summary(exampleAB) |> export()` or `export(exampleAB, summary = TRUE)`)

## Changes

- `rci()`: removed the Hageman et al. method as it is not appropriate for single-cases in the current implementation.
- `scdf()`: New argument `phase_starts()`. Which defines the measurement times of the start of each phase. `phase_starts = list(A = 1, B = 10, C = 15)`. It throws an error when a phase start is defined where no corresponding measurement-time exists. `phase_starts` is a generalization of `B_start`.
- `rand_test()`: New option for `statistic`: `SMD` calculates the standardized mean difference as Hedge's g with Durlak correction. `W-test` computes Wilcoxon tests and compares average W statistics. `T-test` computes T-tests and compares average t-Values. `NAP` and `NAP decreasing` for Non-overlap of all pairs.
- `nap()`: added Cohen's d and R-Squared effects.
- `export()`: `select` argument for `nap`
- `coef.sc_hplm()`: new `casewise` argument. If set TRUE, returns the effect estimations casewise.
- `print.sc_hplm()`: new `casewise` argument. If set TRUE, returns the effect estimations casewise.
- `export.sc_hplm()`: new `casewise` argument. If set TRUE, returns the effect estimations casewise.
- `export.scdf()`: new `summary` argument. If TRUE, returns a summary.
- `hplm()`: new arguments `random_trend`, `random_level`, and `random_slope` to selectively add respective random slope effects to the model.

## New examples

- `Parker2009b`

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
- Various changes and optimization of the ui

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
- The `%>%` operator has been imported and exported from the magrittr package. Now that R 4.1 has a
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


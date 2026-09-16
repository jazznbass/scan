# scan 0.69.0

## Breaking changes

- Removed the `mvar` argument from `smd()`. It had no effect, as standardized mean differences do not use the measurement-time variable.
- Renamed the `cdc()` result fields `cdc_be` and `cdc_b` to `cdc_exc` and `cdc_nb`, the names the help page has always documented.
- Renamed the `pand(method = "minimum")` result field `perc_overlaps` to `perc_overlap`, to match `method = "sort"` and the documentation.
- `scdf()` rejects a phase design that is defined in more than one way, instead of silently letting one definition win.
- Regression models across several cases reject cases with differing phase designs and name the case, instead of failing later when the case data are combined.
- Selecting an unknown case with `$` or `[` raises an error instead of returning an scdf whose case is `NULL`. Such an object looked valid and failed later with an unrelated message.
- `plm()` models autocorrelation along the measurement-time variable, as `hplm()` already did. For cases whose measurement times are not consecutive the results change, because the gaps were previously ignored and the observations treated as equally spaced. Results for consecutive measurement times are unchanged.
- `estimate_design()` warns and falls back when `s` can not be estimated. The between case variance is no longer taken from the spread of the estimated start values alone, which for cases that do not differ at baseline — data simulated with `design(random_start_value = FALSE)`, a single case, two cases — consists of estimation error only. In these situations `s` is now set equal to the standard deviation of the error, which fixes the reported reliability at 0.5 by construction and expresses the effects in units of the variation within a case. Three warnings say so, and providing `s`, `rtt` or `error` is recommended for such data.
- `estimate_design()` returns different reliabilities than before. The `rtt` of a case is now estimated as the quantity `random_scdf()` generates data from, so designs estimated with an earlier version of scan, and data simulated from them, are not reproduced. The entry under *Simulation and power analysis* explains the change.

## New features

- `design(error = ...)` sets the standard deviation of the measurement error directly, as an alternative to `rtt`. The reliability is derived from it as `rtt = s^2 / (s^2 + error^2)`, so the resulting design object is the same as one built with that reliability, and the two arguments can not be given together. `error` is the quantity a study reports as residual variation, while `rtt` relates that variation to the between case variance `s^2` — for a single case, or for cases that hardly differ at baseline, `error` is the parameter that can be pinned down.
- `estimate_design(error = ...)` sets the standard deviation of the measurement error instead of `rtt`, mirroring the new argument of `design()`. The reliability of each case is derived from it as `s^2 / (s^2 + error^2)`. Both arguments now also accept one value per case.
- `fill_missing()` gained a `mark` argument, adding a logical variable `interpolated` that flags every measurement containing an interpolated value.
- `autocorr()` gained an `na.action` argument, which allows to compute autocorrelations from incomplete series.
- `plm()`, `hplm()`, `mplm()` and `bplm()` record the names of their level and slope dummy variables as attributes of the returned object. Predictors are renamed for printing by matching these names exactly, so covariates such as `intervention` or `phase_length` are no longer renamed as if they were dummy variables.

## Bug fixes

### General

- Argument checks accept vectors where the function documents them. The range check used `&&`, which requires a single value since R 4.3, so `design(extreme_prop = c(0.1, 0.3, 0.5))` and `design(missing_prop = ...)` with one value per case — both described on the help page — stopped with `'length = 3' in coercion to 'logical(1)'`, a message naming neither the argument nor the function. A missing value in such an argument is now rejected as well.
- Functions that validate their arguments can be called programmatically again. The argument check looked up the function definition by the name under which the function had been called, which fails whenever there is no such name. `do.call(plm, args)` stopped with `first argument has length > 1`, and calling a function through another one, as in `lapply(list_of_scdf, plm)`, with `object 'FUN' of mode 'function' was not found`. Affected were `plm()`, `hplm()`, `mplm()`, `bplm()`, `pand()`, `tau_u()`, `cdc()`, `corrected_tau()`, `between_smd()`, `rand_test()`, `design()` and `add_dummy_variables()`.

### Data structures and data preparation

- `read_scdf()` rejects file types it can not read. A type matching none of the import branches left the internal data object unassigned, and R then resolved that name in the user's workspace: with an object of that name present, the function returned it as a single-case scdf instead of the file's content, without any warning. An explicitly given `type` is now matched case-insensitively as well, so `type = "CSV"` works.
- `write_scdf()` writes to the console when `filename` is `NULL`, its documented default. `utils::write.table()` takes a character string or a connection, so the default stopped with `argument is of length zero` and the console output was only reachable by passing `filename = ""` by hand.
- `convert()` quotes the phase names in the generated code. Phase names are arbitrary strings in an scdf, but they were written as if they were syntactic R names, so a name containing a space or a hyphen, a name that is a number, or a reserved word such as `if` produced code that did not parse. `convert()` itself reported no problem, and the error only surfaced when someone ran the generated file.
- `convert()` keeps the position of the measurement-time variable. When its values are `1, 2, ... n` the variable is left out of the generated code, because `scdf()` recreates it; but `scdf()` appends it after the other variables, so for data whose measurement times are not the last variable the columns came back in a different order, as in `Leidig2018` and `exampleAB_add`. It is now left out only where that position matches. Missing values in the measurement times no longer stop the function with `missing value where TRUE/FALSE needed`.
- `convert(inline = TRUE)` writes one section per phase of the series. The measurements were grouped by the levels of the phase variable instead of by the sections of the series, so in any design where a phase name recurs — `A B A B` and the other reversal designs — all sections sharing a name were merged into one and the measurements came back in a different order. The generated code reported no problem and produced a valid scdf holding the wrong data.
- `convert(inline = TRUE)` no longer leaves a trailing comma in the generated call. The comma separating the measurements from the argument definitions was written unconditionally, while the definitions added their own only in the other mode, so a case with the default variable names and no case name produced `scdf(..., )` and the generated file stopped with `argument is missing, with no default`.
- Fixed bugs in `scdf()` phase definitions.
- Fixed `scdf(phase_starts = ...)` for data with repeated measurement times, which failed with an uninformative comparison error.
- Fixed case naming in `combine()` / `c()`.
- Fixed custom level-2 IDs and prevented column overwrites in `as.data.frame.scdf()`.
- `as_scdf()` keeps every case a data frame even when a single variable remains after removing the case variable, and checks the case variable for missing values also when it had to be created.
- Fixed logical row filters and empty case selections in `subset.scdf()`.
- Fixed long expressions and access to caller-local variables in `transform()` helpers.
- Corrected the centering position in `center_at()` and prevented out-of-range replacements in `set_na_at()`.
- Fixed `moving_mean()` and `moving_median()` for series shorter than the smoothing window, which failed with an indexing error. The values are now returned unchanged with a warning.
- Fixed automatic phase naming in `select_phases()` to use each case's own phase names, and the construction of combined phase names when phases are selected by name.
- `outlier()` treats each phase as the section of the series it occupies, not as everything carrying the same label. With repeated labels — an ABAB design, for example — the values of every A section were collected for each of the two A phases, so the filter marking the outliers became twice as long as the case. Used as a row index, such a filter adds a row of missing values for every surplus position: the returned data held `NA` rows, the wrong measurements were removed, `dropped.mt` contained missing values and `dropped.n` counted outliers twice. Affected `method = "MAD"`, `"SD"` and `"CI"`; `"Cook"` builds its filter from the regression model and was correct.
- `outlier()` handles missing values in the dependent variable. For `method = "MAD"`, `"SD"` and `"CI"` a single missing value made both bounds of its phase `NA`, so every measurement of that phase was neither inside nor outside them: used as a row index, the whole phase was replaced by rows of missing values and `dropped.n` became `NA`. For `"Cook"` the distances come from a regression fitted on the complete cases, so the filter was shorter than the case and was recycled over it, removing arbitrary measurements or failing with `arguments imply differing number of rows`. Missing values are now ignored when the bounds are computed, are never counted as outliers, and Cook's distances are matched to the measurements they belong to.
- `sample_names()` rejects a type it does not know. `"Male"`, `"m"` or any typo matched none of the four branches and the function returned an empty character vector without a word, which then showed up as missing case names somewhere else entirely.
- `combine()` and `c()` make duplicated case names unique and warn about it. Two studies whose cases carry the same names produced an scdf in which those cases could not be told apart: `add_l2()` matches its level-2 data by case name and gave both cases the same row, while `select_cases()` and `x$name` returned only the first of them. Cases without a name are unaffected and are still numbered when printed. `combine()` without any argument now says so instead of failing with `subscript out of bounds`.
- `select_cases()` applies every selection to the same object instead of one after the other. Each argument was used to subset the scdf on its own and the results were then appended, which inverted a negative selection given as several arguments: `select_cases(exampleAB, -Johanna, -Karolina)` returned four cases — both of the excluded ones among them, and one case twice — where `-c(Johanna, Karolina)` returns the one remaining case. Mixing a positive and a negative selection now raises an error, and a call without any selection says so instead of failing with `subscript out of bounds`.
- `select_cases()` resolves its arguments in the environment it was called from, so case names held in a variable are found when the call sits inside a function, not only at the top level.
- `set_vars()`, `set_dvar()`, `set_mvar()` and `set_pvar()` reject a variable that is not part of every case. The name was written into the scdf attribute unchecked, so a typo surfaced only in the next analysis, with a message naming neither the variable nor the place it was set — and in the overlap indices not at all, since they build their tables from a column that does not exist. Something other than a single variable name, and an object that is not an scdf, are rejected as well.
- `batch_apply(simplify = TRUE)` labels each row with the case it came from. The `case` column was built by dividing the total number of rows by the number of cases, which holds only when every case contributes equally many rows. Where the division did not come out even, the call failed with `replacement has 4 rows, data has 5`; where it did — one case returning two rows and another four, for instance — the rows were labelled with the wrong cases and nothing pointed it out. The rows are now counted per case before they are combined.
- `batch_apply(simplify = TRUE)` keeps the names of the results readable. The combined table was built with the default `check.names`, which turned `Std. Error` into `Std..Error` and `Pr(>|t|)` into `Pr...t..`, and its `rownames` column reported `X.Intercept.` where the model called the parameter `(Intercept)`.
- `batch_apply()` evaluates its expression in the environment it was called from. The search continued from inside `batch_apply()` instead, so objects defined within a function were not found: a helper function or a threshold defined next to the call failed with `could not find function`, while the same code worked at the top level, where the search reaches the global environment anyway.
- `rescale()` takes its variable names as characters or from a variable as well as as object names, the way `select_cases()` documents it, and rejects a name it cannot use. The names were read from the unevaluated call, so `rescale(dat, "values")` looked for a column whose name carries the quotation marks, and a misspelled or non-numeric name surfaced only deep inside the computation as `'x' is NULL` or `undefined columns selected`, naming neither the variable nor the function. A variable that is missing in one of the cases or is not numeric now raises an error naming it.
- The `mad` column of `outlier(method = "MAD")` reports the value the bounds are built from. It was computed with `constant = 1`, while the bounds use the scaled median absolute deviation, so `md` plus or minus `criteria` times `mad` read off the matrix gave bounds too narrow by a factor of 1.4826. The bounds and the outliers they identify are unchanged.

### fill_missing()

- Missing values of the measured variables are interpolated again. They had been left untouched, so only absent measurement times were filled.
- Measurement times missing at the beginning or the end of a series are now determined as well, and supplied measurement times are preserved instead of being rounded.
- A case whose measurement times remain unknown is returned unchanged with a warning. Such observations were previously pushed to the end of the series and replaced by interpolated values.
- Fixed interpolation for insufficient data, repeated measurement times, cases with fewer than two observations, and rows that are out of time order.
- The returned rows are renumbered instead of being labelled `NA`.

### Effect sizes and overlap indices

- `describe()` handles phases without observed values. Minimum and maximum came back as `Inf` and `-Inf` with a warning, the mean as `NaN`, and the trend stopped the whole call with `0 (non-NA) cases` from `lm.fit`, so a single case with an unmeasured phase made the descriptives unavailable for the entire study. The number of measurements and the number of missing values are still reported, every other statistic is `NA`, and the trend is computed where at least two values were observed. The trend no longer depends on the global `na.action` setting either.
- `rand_test(statistic = "Slope A-B")` computes the difference in the direction it names. It passed the method of `"Slope B-A"` on, so both slope statistics returned the same value and the p value of the opposite direction. For a rising series `"Slope A-B"` reported p = 0.00 where p = 1.00 is correct.
- The function behind the two slope statistics of `rand_test()` is registered under the name the function looks up. It had been stored under a different one, and only worked because the same line of code left an object of a matching name in the package namespace, which `match.fun()` picked up as a fallback. Removing that stray assignment would have silently disabled both slope statistics.
- `rand_test()` checks the `limit` argument against the length of the series. When a case had fewer measurements than `limit` requires for both phases, the sequence of admissible start points was built backwards and used anyway, so phase A could fall below the minimum without any notice: for eight measurements, `limit = 5` and `limit = 3` produced the same start points 4, 5 and 6. Such a case now stops with a message naming it, and a `limit` below one is rejected.
- `rand_test(startpoints = list(...))` assigns one set of start points per case, as the help page describes. The whole list was handed to every case instead, which stopped with `non-numeric argument to binary operator`. Start points outside the range of measurements are now rejected as well; they produced a p value of `NA` or silently emptied a phase.
- `rand_test()` draws from the admissible start points also when only one of them is left for a case. `sample()` treats a single number as a range, so such a case drew from `1` to that number instead. This affected samples of start points, not the complete enumeration.
- `rand_test()` reports `NA` with a message when the observed statistic or the randomization distribution is not a finite number, instead of building a p value from such values. This happens for the standardised mean differences when a phase has no variance. `Inf >= Inf` counted as a hit, so a constant baseline with `statistic = "SMD glass"` returned p = 0.55 from six infinite values out of eleven, indistinguishable from a real result, while `NaN` values gave `NA` without any notice.
- `rand_test(statistic = "T-test")` no longer stops inside `t.test()` when a random split has no variance, which ended the whole call with `data are essentially constant`. Such a permutation counts as not computable and the p value follows the rule above.
- `Z` and `p.Z.single` are `NA` when the randomization distribution has no variance, instead of `NaN` from a division by zero.
- `print()` and `export()` for a `rand_test()` result survive a degenerate randomization distribution. A p value of `NA` stopped the output with `missing value where TRUE/FALSE needed`, and a distribution whose values are all identical stopped it inside `shapiro.test()` even when the p value itself was sound. The normality test is now computed from the finite values and skipped with a note when there are too few or they do not vary.
- `export()` for a `rand_test()` result carries a footnote naming the number of cases and, where the cases are named, the case names. The block meant to build it was empty.
- Preserved the phase selection before missing values are removed in `pnd()`, `pem()`, `nap()`, `pand()`, `ird()`, `corrected_tau()`, `cdc()` and `rand_test()`, and reject or skip cases without observed values in a selected phase.
- Handled empty phases in PEM and NAP, excluded unusable cases from PAND and IRD with correct case counts, and handled insufficient data in `corrected_tau()` and `cdc()`. CDC overall results remain missing when any case is unevaluable.
- Fixed phase selection and missing-data handling in `pet()`. Two baseline observations allow PET and its binomial test, while the PET confidence interval requires at least three.
- Fixed phase selection and phase-B counts in `rci()`, which now requires at least two observed values per selected phase.
- Fixed tie handling in `pand(method = "sort")`. The `decreasing` argument no longer reverses the phase tiebreak, so both directions are treated symmetrically.
- Fixed `export()` for `pand(method = "minimum")`, which failed because it accessed statistics that only exist for `method = "sort"`.
- `pem()` counts the exceeding measurements once and uses that count for the percentage, the binomial test and the chi-squared test, which previously reconstructed it from the percentage. A `FUN` returning `NA` is reported with a warning instead of failing inside `binom.test()`, and all result columns are numeric even when no test was computed.

### Simulation and power analysis

- `estimate_design()` keeps a reliability that was passed to it. `overall_rtt` is documented as being ignored when `rtt` is set, but the estimate overwrote the given value in every case, `overall_rtt` being `TRUE` by default — including in the example on the help page, where `estimate_design(scdf, rtt = 0.8)` returned a design with a different reliability than the one asked for.
- `estimate_design()` estimates the reliability of the measurements as `s^2 / (s^2 + error variance)`, the quantity `random_scdf()` uses under that name. It previously reported the R squared of the piecewise regression, `var(fitted) / (var(fitted) + var(residuals))`, which measures how much of the variation within a case trend, level and slope explain — something else entirely. The estimate therefore followed the size of the effect instead of the precision of the measurement: a case without any effect came back with a reliability near zero, which `random_scdf()` turned into an exploding error term, while a strong effect produced a reliability near one. Simulating with a given reliability, estimating it back and simulating again now reproduces the value: 0.50, 0.70, 0.80 and 0.95 are recovered as 0.53, 0.73, 0.82 and 0.96, and the estimate no longer moves when the level effect is varied from 0 to 2.
- `estimate_design()` estimates the error variance with the degrees of freedom of the model. `var(residuals)` divides by the number of measurements less one, although four parameters were estimated from them, which made the error variance too small and the reliability too large. Simulating with a reliability of 0.50, 0.70, 0.80 or 0.95 and estimating it back now returns 0.51, 0.71, 0.80 and 0.95, where the uncorrected estimate returned 0.53, 0.73, 0.82 and 0.96.
- `estimate_design()` subtracts the uncertainty of the estimated start values from the between case variance. The variance of the estimated start values is the variance of the true start values plus the sampling variance of these estimates, so `s` was systematically too large — the more so the fewer measurements a case has, since the start value is the intercept of the regression, extrapolated to the measurement time before the first one. The mean squared standard error of the intercepts is now subtracted. When what remains is not positive, the cases do not differ beyond the precision of their estimates, and `s` falls back to the standard deviation of the error with a warning rather than being built from estimation error.
- `estimate_design()` no longer builds a design object out of an undefined `s`. With one or two cases and no `s`, the value stayed `NULL`, and dividing by it turned the trend, level and slope effects into empty vectors while the element `s` disappeared from every case. The resulting design object looked ordinary and made `random_scdf()` produce a case of nothing but missing values. Such data now take the documented fallback, and an `s` that is zero, not finite or not a number is rejected.
- `design(B_start = ...)` works without `mt`. The help page documents `mt = 20` as the default, but the argument defaulted to `NULL`, so exactly the call the help page describes — `B_start = 6`, assigning the first five measurements of each case to phase A — stopped with `replacement has length zero`. An `mt` given explicitly still takes precedence, and `phase_design` is unaffected.
- `power_test()` tests the falling direction with its `rand_slope_decrease` method. It used the same statistic as `rand_slope`, so both methods returned the same power.
- `design()` hands the whole `extreme_range` to every case. The range is a pair of a lower and an upper bound, but it was treated like the arguments that carry one value per case, so for exactly two cases the first case received the lower bound and the second the upper one, each without a counterpart. `random_scdf()` then drew extreme values against an undefined upper bound, warned `NAs produced` and wrote missing values into the simulated data — seven of forty measurements with the default settings. A list of pairs now assigns one range per case.
- `random_scdf()` keeps count data as counts when extreme values are added. The extreme values are drawn as continuous numbers and added to the simulated scores, but only negative results were corrected, so binomial and Poisson data came back with decimal places, and binomial counts could exceed the number of trials: with `n_trials = 10` and `extreme_range = c(5, 8)`, 23 of 40 measurements lay above 10 and the largest was 15.43. Passed to `plm(family = "binomial")` such data produced proportions above 1. Counts are now rounded and binomial values are capped at the number of trials, while missing values created by `missing_prop` are preserved.
- `random_scdf(3)` builds the three cases it was asked for. The number was noted, the design was then created without it, and the count overwritten by the result, so the call warned about the unnamed argument and returned a single case. The warning about naming the argument `n` remains.
- `random_scdf()` rejects a `random_names` value it does not know. `"Male"`, `"m"` or any typo produced an scdf without any case names and no indication that the argument had been ignored.
- `design()` rejects an `extreme_range` whose first value is not below the second. The help page states that the procedure fails in that case, but `runif()` simply returned `NaN` with a warning, so half the simulated measurements silently became missing values — twenty of forty with `extreme_range = c(-3, -4)`. Ranges of the wrong length or containing a missing value are rejected as well.
- `design()` rejects phase lengths that are not whole numbers of at least one. A length of zero, a negative or a fractional length was accepted and the design object returned, and the call only failed later inside `random_scdf()` with `invalid 'times' argument`, a message naming neither the phase nor the case. `B_start = 1`, which leaves phase A empty, reached the same dead end.
- `power_test()` reports the p values of its binomial tests with three decimals. `p_power` and `p_alpha` were rounded to whole numbers and could therefore only be 0 or 1: an alpha error rate of 2.5 % tested against a threshold of 5 % was reported as `p_alpha = 0` where the test gives p = 0.40, and a rate of 5.0 % as `p_alpha = 1` where p = 0.68. Both readings suggest the opposite of what the test says. `p_correct` was already computed correctly.
- `power_test(ci = ...)` uses the requested level for all three confidence intervals. The interval for the correct proportion was built without `conf.level` and was therefore always a 95 % interval, while the intervals for power and alpha error followed the argument, so a table could hold intervals at two different levels under one heading.
- `power_test()` no longer stops when a binomial test or a confidence interval is requested while `alpha_test` or `power_test` is switched off. The missing value was handed to `binom.test()` and ended the call with `'x' must be nonnegative and integer`.
- The print method for a `power_test()` result shows the p value for the correct proportion when it was asked for. The block was guarded by `binom_test_power` instead of `binom_test_correct`, so `binom_test_correct` on its own computed the value but never printed it. Using the shortcut `binom_test = TRUE` hid the mistake, because it sets all three thresholds at once.
- `power_test()` accepts unnamed functions in its `method` argument. The help page describes `method` as a list whose elements can be functions, but the rows of the result table are taken from the names of that list, so a list without names ended the call with `replacement has 1 row, data has 0` before any simulation was run. Unnamed elements now receive the placeholder names `function1`, `function2`, and so on, while named elements keep their name.
- `power_test()` reports the alpha to beta ratio as missing when it is not defined. The ratio was formatted without any check, so the column could hold `1:NA` when `alpha_test` or `power_test` was switched off, `1:Inf` whenever the observed alpha error proportion was zero — the usual case for a conservative method at `n_sim = 100` — and `1:NaN` when the power was 100 % at the same time. All three stood among rows holding real ratios, with nothing to mark them as undefined.

### Regression and correlation

- Fixed `tau_u()` for cases with fewer than two observed values in a phase, which produced a plausible looking but invalid Tau-U. Such cases now return `NA` with a warning, and the meta analysis returns `NA` when any case is unevaluable.
- Fixed `tau_u(ci = NULL)`, which failed in the meta analysis although the documentation offers `NULL` as a way to suppress confidence intervals. The `ci` argument is now validated and accepts `NULL`, `NA`, or a value between 0 and 1.
- Fixed the zero variance check in the internal Kendall tau computation, which tested the first variable twice and never the second. A constant second variable silently returned `NaN` and now issues a warning. Computing tau with fewer than two data points raises an error.
- Fixed `trend()` for custom models with more than one predictor, which silently reported the second raw coefficient in the `Beta` column. Such models now raise an informative error.
- `autocorr()` reports missing values in the dependent variable with an informative message pointing to `fill_missing()`, instead of failing inside `acf()`, and handles phases with fewer than two observations, which failed with an indexing error.
- `plm()` computes the F test and R squared from the same residual sum of squares. R squared previously used `var(residuals)`, which is only equivalent for ordinary least squares; with `AR > 0` the residuals are not mean free and the two statistics referred to slightly different quantities. Results for `AR = 0` are unchanged.
- Fixed `hplm(lr.test = TRUE)`, which derived the models for the likelihood ratio tests from the text of the random effects formula instead of the random effects the model actually estimated. Two failures followed from this. Names containing a `1` were mangled, because the random intercept was removed by replacing every `1` with `-1`: `phaseB1` became `phaseB - 1`, which either failed with `object not found` or silently tested a different random effect. And a user supplied formula with an implicit intercept, such as `random = ~ mt + phaseB | case`, produced fewer tests than there were random effects, so printing the result failed. The tests are now built from the estimated random effects, one per effect, in the order the output uses.
- `hplm()` reports `random.slopes` in the result correctly when random effects were requested through `random_trend`, `random_level` or `random_slope` rather than through `random.slopes` itself.
- `add_l2()` keeps every case a valid part of the scdf when a case has no matching row in the level-2 data. Such cases lacked the level-2 variables entirely, which made the scdf unusable and failed later when the case data were combined. They now receive `NA`, so the case is dropped from the model by `na.omit` instead.
- `hplm()` warns when cases are dropped from the model because of missing values. The reported number of cases refers to the data passed in, which could differ from the number actually estimated without any notice.
- Fixed `mplm()` for data with missing values. The dependent variables were combined into a matrix outside the data and the null model was fitted without the data, so the null model used all measurements while the full model dropped the incomplete ones. `anova()` and `print()` then failed with `models were not all fitted to the same size of dataset`. The dependent variables are now part of the model formula, and the null model is fitted to the rows the full model used.
- Fixed `mplm(formula = ...)`. A user supplied formula was evaluated in the environment it was written in, where the response matrix did not exist, so `mplm(formula = y ~ mt + phaseB + interB)` failed with `variable lengths differ`. The response is now addressed by the names of the dependent variables.
- `bplm()` keeps a random effects formula passed through the `random` argument. It was replaced without notice whenever `random_trend`, `random_level` or `random_slope` was set, although `random` is documented to overwrite the automatically created random part of the model.
- `fetch()` reports an unsupported `what` instead of returning `NULL`. All four methods returned the requested element from inside an `if` without an `else`, so anything other than `"model"` — a typo, a different capitalisation, an element name such as `"data"` — gave an invisible `NULL` with no indication that the value was not supported. A `what` that is not a single string is rejected as well.
- `anova()` for `hplm()` objects passes additional arguments on unchanged. The call was assembled as text and the arguments were inserted by their value, so a character argument lost its quotes and `anova(model, type = "marginal")` failed with `object 'marginal' not found`, while more than one additional argument failed with `subscript out of bounds`.
- `plm()` passes additional arguments on to `nlme::gls()` when `AR > 0`, as `...` is documented to do. They were dropped without notice, so arguments such as `weights` had no effect on the model and a misspelled argument went unnoticed instead of raising an error.
- `plm()` and `hplm()` check the measurement times before modelling autocorrelation. `nlme::corARMA()` requires whole numbers that are unique within a case: `hplm(ar > 0)` stopped with `covariate must have unique integer values within groups for "corARMA" objects`, and `plm()` ignored the measurement times altogether. Both now report the problem and set the autoregression to 0.

### Messages and printed output

- `print()` for an scdf works with `cols = "main"`. The names of the dependent, phase and measurement-time variable were read with `attr()`, but an scdf keeps them inside a single `scdf` attribute, so all three came back `NULL`, every column was dropped, and the call stopped with `'names' attribute [1] must be the same length as the vector [0]`. The documented setting `options(scan.print.cols = "main")` broke every scdf print in the same way. Selecting a single column, as in `cols = "values"`, reduced each case to a vector and stopped with `incorrect number of dimensions`.
- Long messages and warnings are truncated at a word boundary instead of in the middle of a word.
- The note on the variables used in an analysis no longer fails when an object does not carry all three variable attributes, and reports only the attributes that are set.
- `export()` for `hplm()` and `bplm()` uses a footnote passed through the `footnote` argument. It was replaced by the automatically generated footnote without notice. `export()` for `plm()` and `mplm()` was already correct.
- `print()` and `export()` report the AIC of a `plm()` model with `AR > 0`. The value was taken from a list element that only `glm` objects carry, so it showed as `NA` for the `gls` models that are fitted when autocorrelation is modelled.
- `print()` for `plm()` objects works with `ci = FALSE`, which the help page offers. It stopped with `object 'param_filter' not found`, because the variable is only created when confidence intervals are computed, and for Poisson and binomial models additionally with `$ operator is invalid for atomic vectors`.
- `export()` for `plm()` objects derives the column groups of the table from the table itself instead of assuming its shape. Three things went wrong before. With `ci = FALSE` the confidence interval group was placed anyway, which labelled the `SE` and `t` columns as confidence limits and made Poisson models fail; `ci = TRUE` was labelled `CI(100%)` instead of `CI(95%)`. The group for the odds ratio limits was set for Poisson but not for binomial models, so every binomial export failed with the kable engine and lost its labels with the gt engine. And the kable groups assumed exactly one R squared column, so `r_squared = "none"` and `r_squared = c("delta", "partial")` failed. Of the 108 combinations of family, `ci`, `q`, `r_squared` and export engine, 70 either failed or produced a mislabelled table.

## Documentation

- `scdf()`: the help page described a priority order for competing phase-design definitions that did not match the behaviour.
- `overlap()`: documented that PAND is reported for `method = "sort"` while IRD is based on `method = "minimum"`, so the two columns are not algebraically linked. Removed the `design` entry from the documented return value, which the function never returned.
- `trend()`: the `model` argument no longer lists `phase` as an available parameter, as phase terms cannot be estimated within a single phase.
- `cdc()`: removed the `phases` entry from the documented return value, which was never returned.
- `rescale()`: the `...` argument documents that the variables can be named as objects or as characters.
- `outlier()`: MAD is the median absolute deviation, not the "mean average deviation". The help page now says so and adds that `criteria` refers to the scaled deviation returned by `stats::mad()`.
- `hplm()`: the `data.l2` argument requires a column named `case`, not `cases` as the help page stated.
- `hplm()`: documented that the likelihood ratio test accompanying the intraclass correlation tests a variance against zero, a parameter at the boundary of its parameter space, so the reported p value is conservative.
- `mplm()`: the `formula` argument now states that the response is the `cbind()` of the dependent variables, e.g. `cbind(dv1, dv2) ~ 1 + mt + phaseB + interB`.
- `bplm()`: the documented return value `mcmglmm` is named `mcmcglmm`, the description of `formula` referred to the hplm model, and an example announced a random slope while setting `random_level`.
- `anova()`: the example for Poisson models compared them with a Gaussian model fitted to different data and a different response variable, which `anova.glm()` silently dropped again with a warning.
- `plm()` and `hplm()`: the `AR` and `ar` arguments state that the measurement times must be whole numbers and unique within a case.
- `plm()`: the documented return value lists the elements `contrast`, `var_trials`, `dvar_percentage` and `data`, which were returned but not described. For a binomial regression with `dvar_percentage = FALSE`, `data` holds the modelled proportions rather than the counts that were passed in.
- `power_test()`: the documented return value names the class `sc_power` that the function actually sets, rather than describing the result as a data frame. Like every other `sc_*` object it does not inherit from `data.frame`.

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


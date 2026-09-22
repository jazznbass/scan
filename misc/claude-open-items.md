# Open items from the code review (Claude, September 2026)

Deferred deliberately. Confirmed by test unless marked otherwise.

## Deferred: multi-element caption in export()

The footnote half of this item is solved: every export method now passes its
argument through `.footnote()`, which decides by `NULL` / a length-one `NA` /
anything else, so a vector footnote works everywhere and `NULL` suppresses it.

`caption` still uses `if (is.na(caption))` in 24 places, so

    export(pand(exampleAB), caption = c("a", "b"))

stops with `the condition has length > 1`, and `caption = NULL` with
`argument is of length zero`. The same helper would do:

    caption <- .caption(caption, "Percentage of all non-overlapping data")

`grep -rn "if (is.na(caption))" R/` lists the places.

## Deferred: repeated phase labels in regression models

A design written as `scdf(c(A = .., B = .., A = .., B = ..))` yields only two
factor levels, because `scdf()` builds the factor with
`levels = unique(names(phase_design))`. In `.create_slope_dummies()`
(`R/plm_contrasts.R`) the slope dummy is then filled via
`which(phase %in% selection_phases)`, which selects both A blocks at once, so
`mt_dummy <- mt[id] - mt[id[1]]` runs across the gap between them.

`describe()` handles this case with `rename_phase_duplicates()`
(`R/describe.R`, line 53); the regression functions do not.

Open question for the author: should such designs be supported, or is
`A1/B1/A2/B2` the required spelling? Not investigated further.

## Noted: no guard against over-parameterised random effects in hplm()

`hplm(exampleA1B1A2B2, random.slopes = TRUE, lr.test = TRUE)` has 3 cases and 8
random effects, that is 36 variance parameters, and with `lr.test = TRUE` eight
further restricted models are fitted. `lme()` iterates for minutes without any
feedback. A warning when the number of variance parameters exceeds the number of
cases would help.

## Noted: inconsistent coefficient extractors

`coef.sc_plm()` returns `summary(object$full.model)$coef` with the raw
predictor names, while `coef.sc_hplm(casewise = TRUE)` renames them via
`rename_predictors()`. Defensible for a low-level extractor, but the two
methods behave differently.

## Declined

- `.check_scdf()`: guarding the phase-design block (lines 79-88) against
  errors found earlier stays as it is.

## Noted: bplm() and MCMCglmm default priors

`bplm()` passes no prior to `MCMCglmm()`. With more than a random intercept on
unscaled data the sampler regularly stops with

    ill-conditioned G/R structure (CN = 4.6e+15): use proper priors if you
    haven't or rescale data if you have

Observed with `bplm(exampleAB_50, random = ~ us(1 + mt):case)`, which fails on
some runs and succeeds on others with identical data and formula, so it is the
starting values, not the model. `random_trend = TRUE` produces the same formula
and has the same behaviour. Options would be a weakly informative default prior
for the G structure, or documenting the `prior` argument of `MCMCglmm()` in
`?bplm` with a worked example. Not a bug; noted as a usability item.

## Noted: combine() turns a NULL names attribute into ""

`Waddell2011` and `Borckardt2014` are single-case scdfs whose cases carry no
name: `names(x)` is NULL. After `convert()` and `source()`, the object created
by `c(study1, info = ...)` has `names(x) == ""`. Both print as an empty string,
but `all.equal()` reports "names for target but not for current", so a round
trip looks broken although the data are identical. Either `combine()` should
keep NULL when no case has a name, or the data sets should carry real case
names. No practical consequence; noted while working on convert().

## Noted: the unused 'output' argument of rand_test()

`rand_test()` still carries `output = NULL` in its signature. It is documented
as "(deprecated and not implemented)" and is never read in the body. Removing
it makes an old call stop with `unused argument`; routing it through
`as_deprecated()` in `check_args()`, the way `design()` handles its renamed
arguments, would warn instead. Decision pending.

## Noted: the dvar / pvar / mvar arguments of the analysis functions

`set_vars()` and its three single setters now reject a variable that is not
part of every case (Bug 71). The same names reach about twenty analysis
functions through their own `dvar`, `pvar` and `mvar` arguments —
`plm(dat, dvar = "valeus")` — and are still written into the scdf attribute
unchecked. The natural place for one shared check is `.prepare_scdf()`, which
every one of these functions passes through, but the argument is resolved
before that call in most of them, so it needs a small reordering per function.
Not started.

## Noted: rtt as an intraclass correlation (estimate_design)

Done so far: the error variance uses the residual degrees of freedom, `s^2` is
corrected by the mean squared standard error of the estimated start values, the
definition of `rtt` is documented in `?design`, and `design(error = ...)` offers
the measurement error directly.

Still open is the cleaner statistical route: estimating `s^2` and the error
variance jointly from a random intercept model, so that
`rtt = tau00 / (tau00 + sigma^2)` — the intraclass correlation. It would replace
two separately corrected moments by one model. `hplm()` is in the package
already, but it requires the same phase design for every case, while
`estimate_design()` works case by case and tolerates different designs, so it
needs a fallback. Worth doing as an option, not as a replacement.

# Open items from the code review (Claude, September 2026)

Deferred deliberately. Confirmed by test unless marked otherwise.

## Deferred: multi-element caption / footnote in export()

`if (is.na(footnote))` / `if (is.na(caption))` breaks with `the condition has
length > 1` as soon as a user passes a vector, although the package's own
default footnotes are multi-element vectors.

    export(pand(exampleAB), footnote = c("first line", "second line"))

Suggested helper in `R/private-utilities.R`:

    # TRUE when an optional export argument was not set by the user.
    is_not_set <- function(x) {
      is.null(x) || (length(x) == 1L && is.na(x))
    }

Replace `if (is.na(footnote))` with `if (is_not_set(footnote))`, same for
`caption`. `filename` is unaffected (always length 1).

footnote, 17 places:

    R/export.sc_desc.R:15          R/export.sc_pet.R:12
    R/export.sc_power.R:11         R/export.sc_tauu.R:27
    R/export.sc_trend.R:12         R/export.scdf.R:19
    R/export.scdf_summary.R:10     R/ird.R:98
    R/pand.R:251                   R/pand.R:311
    R/print-export-bcsmd.R:37      R/print-export-cdc.R:53
    R/print-export-mplm.R:199      R/print-export-outlier.R:57
    R/print-export-plm.R:95        R/print-export-rand-test.R:92
    R/print.export-bctau.R:76

caption: 24 further places, `grep -rn "if (is.na(caption))" R/`

## Deferred: audit message lengths

`notify()` truncates at 100 characters (`wmisc.msg.max`). Since the default
stays at 100, every `warn()` / `notify()` / `abort()` call in the package
should be checked against that limit once.

Known to sit on the edge: the `phase_starts` error in `R/scdf.R` ("First phase
must start at the first measurement time which is X.") is exactly 100
characters for X = 1 and is truncated for any measurement time of four digits.

## Noted as a possible extension

`print.sc_plm()`, `print.sc_hplm()` and `print.sc_cdc()` set the phase, mt and
dv attributes on their result but never call `.note_vars()`, so they do not
report which variables an analysis used. For `plm()` and `hplm()` in particular
a non-default `dvar` changes the whole model without leaving a trace in the
output. Only `print.sc_desc()`, `print.sc_overlap()`, `print.sc_smd()`,
`print.sc_trend()` and the mplm print method report it.

## Declined

- `.check_scdf()`: guarding the phase-design block (lines 79-88) against
  errors found earlier stays as it is.

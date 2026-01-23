# Export scan objects to html or latex

Export creates html files of tables or displays them directly in the
viewer pane of rstudio. When applied in rmarkdown/quarto, tables can
also be created for pdf/latex output.

## Usage

``` r
export(object, ...)

# S3 method for class 'sc_desc'
export(
  object,
  caption = NA,
  footnote = NA,
  filename = NA,
  flip = FALSE,
  round = 2,
  ...
)

# S3 method for class 'sc_nap'
export(
  object,
  caption = NA,
  footnote = NA,
  filename = NA,
  select = c("Case", "NAP", "NAP Rescaled", "w", "p", "d", "R²"),
  round = 2,
  ...
)

# S3 method for class 'sc_overlap'
export(
  object,
  caption = NA,
  footnote = NULL,
  filename = NA,
  round = 2,
  decimals = 2,
  flip = FALSE,
  ...
)

# S3 method for class 'sc_pem'
export(object, caption = NA, footnote = NA, filename = NA, round = 2, ...)

# S3 method for class 'sc_pet'
export(object, caption = NA, footnote = NA, filename = NA, round = 1, ...)

# S3 method for class 'sc_pnd'
export(
  object,
  caption = NA,
  footnote = NA,
  filename = NA,
  select = c("Case", "PND", "Total", "Exceeds"),
  round = 2,
  ...
)

# S3 method for class 'sc_power'
export(object, caption = NA, footnote = NA, filename = NA, round = 3, ...)

# S3 method for class 'sc_smd'
export(
  object,
  caption = NA,
  footnote = NA,
  filename = NA,
  select = c("Case", `Mean A` = "mA", `Mean B` = "mB", `SD A` = "sdA", `SD B` = "sdB",
    `SD Cohen` = "sd cohen", `SD Hedges` = "sd hedges", "Glass' delta", "Hedges' g",
    "Hedges' g correction", "Hedges' g durlak correction", "Cohen's d"),
  round = 2,
  decimals = 2,
  flip = FALSE,
  ...
)

# S3 method for class 'sc_trend'
export(
  object,
  caption = NA,
  footnote = NA,
  filename = NA,
  round = 3,
  decimals = NULL,
  ...
)

# S3 method for class 'scdf'
export(
  object,
  summary = FALSE,
  caption = NA,
  footnote = NA,
  filename = NA,
  cols,
  round = 2,
  ...
)

# S3 method for class 'scdf_summary'
export(object, caption = NA, footnote = NA, filename = NA, round = 2, ...)
```

## Arguments

- object:

  An scdf or an object exported from a scan function.

- ...:

  Further Arguments passed to internal functions.

- caption:

  Character string with table caption. If left NA (default) a caption
  will be created based on the exported object.

- footnote:

  Character string with table footnote. If left NA (default) a footnote
  will be created based on the exported object.

- filename:

  String containing the file name. If a filename is given the output
  will be written to that file.

- flip:

  If TRUE, some objects are exported with rows and columns flipped.

- round:

  Integer passed to the digits argument used to round values.

- select:

  A character vector containing the names of the variables to be
  included. If the vector is named, the variables will be renamed
  accordingly.

- decimals:

  Decimal places that are reported.

- summary:

  If TRUE, exports the summary of an `scdf`.

- cols:

  Defines which columns are included when exporting an scdf. It is
  either a vector of variable names or the string "main" will select the
  central variables.

## Value

Returns or displays a specially formatted html (or latex) file.

## Details

The function uses either the kableExtra or the gt package to create the
tables. The default engine can be set via the option
`scan.export.engine`. Additional options for kable and kable_styling can
be set via the options `scan.export.kable` and
`scan.export.kable_styling`. The default options can be viewed and
modified via `options("scan.export.kable")` and
`options("scan.export.kable_styling")`.

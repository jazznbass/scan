# Export scan objects to html or latex

Export creates html files of tables or displays them directly in the
viewer pane of rstudio. When applied in rmarkdown/quarto, tables can
also be created for pdf/latex output.

## Usage

``` r
export(object, ...)

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
```

## Arguments

- object:

  An scdf or an object exported from a scan function.

- ...:

  Further Arguments passed to internal functions.

- summary:

  If TRUE, exports the summary of an `scdf`.

- caption:

  Character string with table caption. If left NA (default) a caption
  will be created based on the exported object.

- footnote:

  Character string with table footnote. Several strings are combined
  into a footnote of several lines. If left NA (default) a footnote will
  be created based on the exported object. `NULL` or `""` suppress the
  footnote.

- filename:

  String containing the file name. If a filename is given the output
  will be written to that file.

- cols:

  Defines which columns are included when exporting an scdf. It is
  either a vector of variable names or the string "main" will select the
  central variables.

- round:

  Integer passed to the digits argument used to round values.

## Value

Returns or displays a specially formatted html (or latex) file.

## Details

The function uses either the gt or the kableExtra package to create the
tables. The default engine is gt; in a document knitted to latex,
kableExtra is used instead, because gt tables receive neither a number
nor a label there, and for Word output gt is used even when kableExtra
is set, because kableExtra can not write Word tables. The engine can be
set via the option `scan.export.engine`. Additional options for kable
and kable_styling can be set via the options `scan.export.kable` and
`scan.export.kable_styling`. The default options can be viewed and
modified via `options("scan.export.kable")` and
`options("scan.export.kable_styling")`.

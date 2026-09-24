# Summary function for an scdf object

Provides a summary of an `scdf` object, including the number of cases,
measurements per case, and design information.

## Usage

``` r
# S3 method for class 'scdf'
summary(object, all_cases = FALSE, ...)

# S3 method for class 'scdf_summary'
print(x, all_cases = NULL, ...)

# S3 method for class 'scdf_summary'
export(object, caption = NA, footnote = NA, filename = NA, round = 2, ...)
```

## Arguments

- object:

  An scdf object

- all_cases:

  If TRUE, more than 10 cases are summarized

- ...:

  not in use

- x:

  An object of class `scdf_summary`

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

- round:

  Integer passed to the digits argument used to round values.

## Value

An object of class `scdf_summary`.

## Details

The summary includes:

- Total number of cases in the `scdf`.

- A table listing each case with the number of measurements and design.

- Variable names with annotations for phase, measurement-time, and
  dependent variable.

- Additional information and author details if available.

[`summary()`](https://rdrr.io/r/base/summary.html) returns the summary
object; the output is written by its print method. So `summary(scdf)`
shows the summary at the console as before, while
`export(summary(scdf))` and an assignment stay silent.

## Functions

- `print(scdf_summary)`: Print the summary

- `export(scdf_summary)`: Export the summary as html table (see
  [`export()`](https://jazznbass.github.io/scan/reference/export.md))

## Author

Juergen Wilbert

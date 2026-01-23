# Load single-case data from files

Use the `read_scdf` function to load single-case data csv, excel, or
yaml files.

## Usage

``` r
read_scdf(
  file,
  cvar = "case",
  pvar = "phase",
  dvar = "values",
  mvar = "mt",
  sort_cases = FALSE,
  phase_names = NULL,
  type = NA,
  na = c("", "NA"),
  sort.labels = NULL,
  phase.names = NULL,
  ...
)
```

## Arguments

- file:

  Either a character string defining the file to be loaded (e.g.
  `"SC_Anita.csv"` (if left empty a dialog box for choosing will be
  opened) or a data.frame.

- cvar:

  Sets the variable name of the "case" variable. Defaults to `"case"`.

- pvar:

  Sets the variable name of the "phase" variable. Defaults to `"phase"`.

- dvar:

  Sets the variable name of the "values" variable. Defaults to
  `"values"`.

- mvar:

  Sets the variable name of the "mt" variable. Defaults to `"mt"`.

- sort_cases, sort.labels:

  If set TRUE, the resulting list is sorted by label names
  (alphabetically increasing).

- phase_names, phase.names:

  A character vector with phase names. Defaults to the phase names
  provided in the phase variable.

- type:

  Format of the file to be loaded. Either "csv", "xlsx", "xls", "excel",
  "yml" is possible. By default (NA) the type is extracted from the file
  extension.

- na:

  Character vector of strings to interpret as missing values.

- ...:

  Further arguments passed to the respective read function.

## Value

Returns a single-case data frame. See
[`scdf`](https://jazznbass.github.io/scan/reference/scdf.md) to learn
about the format of these data frames.

## Details

The data file must be in a "long" format with one column for case
identifiers, one column for phase identifiers, one column for
measurement time, and one column for the dependent variable values. Each
row represents a single measurement.

## See also

[`read.table()`](https://rdrr.io/r/utils/read.table.html),
[`readRDS()`](https://rdrr.io/r/base/readRDS.html)

Other io-functions:
[`convert()`](https://jazznbass.github.io/scan/reference/convert.md),
[`write_scdf()`](https://jazznbass.github.io/scan/reference/write_scdf.md)

## Author

Juergen Wilbert

## Examples

``` r
## Read SC-data from a file named "study1.csv" in your working directory
# study1 <- read_scdf("study1.csv")

## Read SC-data from a .csv-file with semicolon as field and comma as decimal separator
# study2 <- read_scdf("study2.csv", sep = ";", dec = ",")

## write_scdf and read_scdf
filename <- file.path(tempdir(), "test.csv")
write_scdf(exampleA1B1A2B2_zvt, filename)
dat <- read_scdf(filename, cvar = "case", pvar = "part", dvar = "zvt", mvar = "day")
#> Imported 3 cases
res1 <- describe(exampleA1B1A2B2_zvt)$descriptives
res2 <- describe(dat)$descriptives
all.equal(res1,res2)
#> [1] TRUE
```

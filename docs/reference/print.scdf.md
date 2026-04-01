# Print an scdf

Print an scdf

## Usage

``` r
# S3 method for class 'scdf'
print(
  x,
  cases = getOption("scan.print.cases"),
  rows = getOption("scan.print.rows"),
  cols = getOption("scan.print.cols"),
  long = getOption("scan.print.long"),
  digits = getOption("scan.print.digits"),
  ...
)
```

## Arguments

- x:

  An scdf object

- cases:

  Number of cases to be printed. "fit" fits the number to the current
  screen width.

- rows:

  Number of rows to be printed.

- cols:

  Columns to be printed. "Main" only prints the dependent,
  measurement-time and phase variable.

- long:

  Logical. If TRUE cases are printed in one by a time.

- digits:

  Number of digits.

- ...:

  Further arguments passed to the print function.

## Details

Print options for scdf objects could be set globally:
option(scan.print.cases = "all"), option(scan.print.rows = 10),
option(scan.print.cols = "main"), option(scan.print.long = TRUE),
option(scan.print.digits = 0), option(scan.print.scdf.name = FALSE)

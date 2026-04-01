# Data output: Write single-case data to a .csv-file

This function restructures and writes single-case data into a .csv-file.

## Usage

``` r
write_scdf(data, filename = NULL, sep = ",", dec = ".", ...)
```

## Arguments

- data:

  A single-case data frame. See
  [`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md) to
  learn about this format.

- filename:

  A character string defining the output file name (e.g.
  `"scdf_data.csv"`. If `NULL` (default), the data are written to the
  console.

- sep:

  The field separator string. Values within each row of x are separated
  by this string.

- dec:

  The string to use for decimal points in numeric or complex columns:
  must be a single character.

- ...:

  Further arguments passed to write.table.

## Value

Invisibly returns `NULL`.

## Details

This is a wrapper for the write.table function with predefined
parameters. It converts the single-case data file into a data frame and
writes it to a .csv-file. Each single-case data set is stacked below
each other and an additional column "case" is added to identify the
different single-cases.

## See also

[`write.table()`](https://rdrr.io/r/utils/write.table.html),
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html)

Other io-functions:
[`convert()`](https://jazznbass.github.io/scan/reference/convert.md),
[`read_scdf()`](https://jazznbass.github.io/scan/reference/read_scdf.md)

## Author

Juergen Wilbert

## Examples

``` r
## write single-case data to a .csv-file
filename <- tempfile(fileext = ".csv")
write_scdf(exampleAB, filename)

## write multiple cases to a .csv-file with semicolon as field and comma as
## decimal separator
write_scdf(Grosche2011, filename, sep = ";", dec = ",")

## read_scdf and write_scdf
write_scdf(exampleA1B1A2B2_zvt, filename)
dat <- read_scdf(filename, cvar = "case", pvar = "part",
                 dvar = "zvt", mvar = "day")
#> Imported 3 cases
res1 <- describe(exampleA1B1A2B2_zvt)$descriptives
res2 <- describe(dat)$descriptives
all.equal(res1,res2)
#> [1] TRUE
```

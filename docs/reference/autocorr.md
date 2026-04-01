# Autocorrelation within and across phases

The autocorr function calculates autocorrelations within each phase and
across all phases.

## Usage

``` r
# S3 method for class 'sc_ac'
print(x, digits = "auto", ...)

# S3 method for class 'sc_ac'
export(object, caption = NA, footnote = NA, filename = NA, round = 3, ...)

autocorr(data, dvar, pvar, mvar, lag_max = 3, lag.max, ...)
```

## Arguments

- x:

  An object returned by `autocorr()`

- digits:

  The minimum number of significant digits to be use. If set to "auto"
  (default), values are predefined.

- ...:

  Further arguments passed to the
  [`acf()`](https://rdrr.io/r/stats/acf.html) function.

- object:

  An scdf or an object exported from a scan function.

- caption:

  Character string with table caption. If left NA (default) a caption
  will be created based on the exported object.

- footnote:

  Character string with table footnote. If left NA (default) a footnote
  will be created based on the exported object.

- filename:

  String containing the file name. If a filename is given the output
  will be written to that file.

- round:

  Integer passed to the digits argument used to round values.

- data:

  A single-case data frame. See
  [`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md) to
  learn about this format.

- dvar:

  Character string with the name of the dependent variable. Defaults to
  the attributes in the scdf file.

- pvar:

  Character string with the name of the phase variable. Defaults to the
  attributes in the scdf file.

- mvar:

  Character string with the name of the measurement time variable.
  Defaults to the attributes in the scdf file.

- lag_max, lag.max:

  The lag up to which autocorrelations will be computed.

## Value

A data frame containing separate autocorrelations for each phase and for
all phases (for each single-case). If `lag_max` exceeds the length of a
phase minus one, NA is returned for this cell.

## Details

Autocorrelations are computed using the
[`acf()`](https://rdrr.io/r/stats/acf.html) function from the stats
package. For each single-case in the scdf object, a data frame is
returned containing the autocorrelations for each phase and for all
phases up to the specified lag.

## Functions

- `print(sc_ac)`: Print method

- `export(sc_ac)`: Export results to html

## See also

[`acf()`](https://rdrr.io/r/stats/acf.html)

Other regression functions:
[`bplm()`](https://jazznbass.github.io/scan/reference/bplm.md),
[`fetch()`](https://jazznbass.github.io/scan/reference/fetch.md),
[`hplm()`](https://jazznbass.github.io/scan/reference/hplm.md),
[`mplm()`](https://jazznbass.github.io/scan/reference/mplm.md),
[`plm()`](https://jazznbass.github.io/scan/reference/plm.md),
[`print.sc_bctau()`](https://jazznbass.github.io/scan/reference/corrected_tau.md),
[`trend()`](https://jazznbass.github.io/scan/reference/trend.md)

## Author

Juergen Wilbert

## Examples

``` r
## Compute autocorrelations for a list of four single-cases up to lag 2.
autocorr(Huber2014, lag_max = 2)
#> Autocorrelations
#> 
#> Adam 
#>  Phase Lag 1 Lag 2
#>      A  0.18 -0.40
#>      B  0.00 -0.16
#>    all  0.26 -0.02
#> 
#> Berta 
#>  Phase Lag 1 Lag 2
#>      A  0.08 -0.52
#>      B  0.07 -0.36
#>    all  0.30 -0.01
#> 
#> Christian 
#>  Phase Lag 1 Lag 2
#>      A -0.07 -0.24
#>      B  0.53  0.40
#>    all  0.64  0.55
#> 
#> David 
#>  Phase Lag 1 Lag 2
#>      A -0.40 -0.25
#>      B  0.27  0.35
#>    all  0.45  0.47
#> 
```

# Descriptive statistics for single-case data

The `describe()` function provides common descriptive statistics for
single-case data.

## Usage

``` r
describe(data, dvar, pvar, mvar)
```

## Arguments

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

## Value

A list containing a data frame of descriptive statistics (descriptives);
the cse design (design); the number of cases (N).

## Details

It computes the number of measurements, number of missing values, mean,
median, standard deviation, median average deviation, minimum, maximum,
and trend (slope of dependent variable regressed on measurement-time)
for each phase of each single-case included in an scdf.

n = number of measurements; mis = number of missing vaues; m = mean; md
= median; sd = standard deviation; mad = median average deviation; min =
minimum; max = maximum; trend = weight of depended variable regressed on
time (values ~ mt).

## See also

[`overlap()`](https://jazznbass.github.io/scan/reference/overlap.md),
[`plot.scdf()`](https://jazznbass.github.io/scan/reference/plot.scdf.md)

## Author

Juergen Wilbert

## Examples

``` r

## Descriptive statistics for a study of three single-cases
describe(Grosche2011)
#> Describe Single-Case Data
#> 
#>        Eva Georg Olaf
#> Design A-B   A-B  A-B
#> n.A      6     7   12
#> n.B     13    17    8
#> mis.A    0     0    0
#> mis.B    0     0    0
#> 
#>            Eva  Georg   Olaf
#> m.A      2.677 10.469  6.036
#> m.B      3.435  9.799  5.995
#> md.A     2.945  8.820  6.355
#> md.B     3.310 10.590  5.905
#> sd.A     0.750  4.112  1.524
#> sd.B     1.029  2.089  1.225
#> mad.A    0.541  4.448  1.794
#> mad.B    1.290  2.090  1.416
#> min.A     1.46   5.82   3.02
#> min.B     1.86   5.60   4.53
#> max.A     3.39  17.40   7.69
#> max.B     4.98  12.79   8.14
#> trend.A  0.014 -0.268  0.007
#> trend.B  0.044  0.043  0.000

## Descriptives of a three phase design
describe(exampleABC)
#> Describe Single-Case Data
#> 
#>        Marie Rosalind  Lise
#> Design A-B-C    A-B-C A-B-C
#> n.A       10       15    20
#> n.B       10        8     7
#> n.C       10        7     3
#> mis.A      0        0     0
#> mis.B      0        0     0
#> mis.C      0        0     0
#> 
#>           Marie Rosalind    Lise
#> m.A      52.000   52.267  52.350
#> m.B      72.100   73.250  73.571
#> m.C      68.000   66.429  71.333
#> md.A       53.5     52.0    52.0
#> md.B       72.5     72.0    73.0
#> md.C         69       68      76
#> sd.A      8.287    8.146  10.869
#> sd.B     11.367   13.134  10.644
#> sd.C     12.702   10.486  21.385
#> mad.A    11.119    7.413  10.378
#> mad.B    10.378   10.378  16.309
#> mad.C    17.791   11.861  20.756
#> min.A        39       37      35
#> min.B        47       54      60
#> min.C        51       52      48
#> max.A        63       65      74
#> max.B        85       97      87
#> max.C        87       78      90
#> trend.A  -1.915    0.500  -0.088
#> trend.B  -0.612    0.643   1.929
#> trend.C  -0.194   -2.929 -14.000

## Write descriptive statistics to .csv-file
study <- describe(Waddell2011)
write.csv(study$descriptives, file = tempfile())
```

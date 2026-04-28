# Replacing missing measurement points in single-case data

The `fillmissing()` function replaces missing measurements in
single-case data. It linearly interpolates missing data points between
two existing measurements for all variables except the measurement time
and phase. The measurement time variable is filled with the missing time
points. The phase variable is copied from the previous measurement time
point. If mt values are missing (`NA`), they are also interpolated if
`interpolate_na = TRUE`.

## Usage

``` r
fill_missing(data, dvar, mvar, pvar, interpolate_na = TRUE)
```

## Arguments

- data:

  A single-case data frame. See
  [`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md) to
  learn about this format.

- dvar:

  Character string with the name of the dependent variable. Defaults to
  the attributes in the scdf file.

- mvar:

  Character string with the name of the measurement time variable.
  Defaults to the attributes in the scdf file.

- pvar:

  Character string with the name of the phase variable. Defaults to the
  attributes in the scdf file.

- interpolate_na:

  If set `TRUE`, `NA` values in the measurement time variable are also
  interpolated. Default is `TRUE`.

## Value

A single-case data frame with interpolated missing data points.

## Details

The `fill_missing()` function is designed to handle single-case data
with missing measurement points. It performs linear interpolation to
estimate the missing values based on the existing data points. The
function iterates through each single-case in the provided single-case
data frame (scdf) and identifies gaps in the measurement time variable.
For each gap, it calculates the step size for linear interpolation and
fills in the missing values for all target variables (i.e., all
variables except the measurement time and phase). The interpolated data
points are then added to the single-case data frame, and the final
result is sorted by measurement time. This function is particularly
useful for preparing single-case data for further analysis, such as
calculating overlap indices or conducting randomization tests, where
continuous measurement times are required. It ensures that the data is
complete by filling in the missing measurement points in a systematic
manner.

## See also

Other data manipulation functions:
[`add_l2()`](https://jazznbass.github.io/scan/reference/add_l2.md),
[`as.data.frame.scdf()`](https://jazznbass.github.io/scan/reference/as.data.frame.scdf.md),
[`as_scdf()`](https://jazznbass.github.io/scan/reference/as_scdf.md),
[`batch_apply()`](https://jazznbass.github.io/scan/reference/batch_apply.md),
[`moving_median()`](https://jazznbass.github.io/scan/reference/transform.scdf.md),
[`print.sc_outlier()`](https://jazznbass.github.io/scan/reference/outlier.md),
[`ranks()`](https://jazznbass.github.io/scan/reference/ranks.md),
[`rescale()`](https://jazznbass.github.io/scan/reference/rescale.md),
[`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md),
[`select_cases()`](https://jazznbass.github.io/scan/reference/select_cases.md),
[`set_vars()`](https://jazznbass.github.io/scan/reference/set_vars.md),
[`shift()`](https://jazznbass.github.io/scan/reference/shift.md),
[`smooth_cases()`](https://jazznbass.github.io/scan/reference/smooth_cases.md),
[`standardize()`](https://jazznbass.github.io/scan/reference/standardize.md),
[`truncate_phase()`](https://jazznbass.github.io/scan/reference/truncate_phase.md)

## Author

Juergen Wilbert

## Examples

``` r

## In his study, Grosche (2011) could not realize measurements each
## single week for all participants. During the course of 100 weeks,
## about 20 measurements per person at different times were administered.

## Fill missing values in a single-case dataset with discontinuous
## measurement times
Grosche2011filled <- fill_missing(Grosche2011)
study <- c(Grosche2011[2], Grosche2011filled[2])
names(study) <- c("Original", "Filled")
study
#> #A single-case data frame with two cases
#> 
#>  Original: values mt phase │ Filled: values mt phase │
#>                13  0     A │             13  0     A │
#>              17.4  1     A │           17.4  1     A │
#>                13  6     A │          16.52  2     A │
#>              7.74 10     A │          15.64  3     A │
#>              8.82 13     A │          14.76  4     A │
#>               7.5 15     A │          13.88  5     A │
#>              5.82 36     A │             13  6     A │
#>              6.26 38     B │          11.69  7     A │
#>               5.6 41     B │          10.37  8     A │
#>             12.79 43     B │           9.05  9     A │
#>              9.17 48     B │           7.74 10     A │
#>             10.59 50     B │            8.1 11     A │
#>              9.23 55     B │           8.46 12     A │
#>             10.71 59     B │           8.82 13     A │
#>              7.61 62     B │           8.16 14     A │
#> # ... up to 87 more rows

## An example with multiple interpolated variables

rolf_n <- exampleAB_add
rolf_n[[1]] <- rolf_n[[1]][-c(3,7,8),]
rolf_f <- fill_missing(rolf_n)
study1 <- c("original" = exampleAB_add, "interpolated" = rolf_f)
study1
#> #A single-case data frame with two cases
#> 
#>  original: day wellbeing cigarrets depression phase
#>              1        46         2          7  Base
#>              2        49         5          6  Base
#>              3        49         4          1  Base
#>              4        49         1          4  Base
#>              5        50         2          7  Base
#>              6        47         4          2  Base
#>              7        45         4          6  Base
#>              8        59         0          0  Base
#>              9        58         2          3  Base
#>             10        59         3          6  Base
#>             11        59         2          8  Base
#>             12        43         1          7  Base
#>             13        46         4          9  Base
#>             14        52         5          6  Base
#>             15        55         5          3  Base
#> # ... up to 25 more rows
#> # One more case

## Example with missing NAs in measurement time
Maggie2 <- random_scdf(design(level = list(0,1)), seed = 123)
Maggie2_n <- Maggie2
Maggie2_n[[1]][c(5,12:14,20), "mt"] <- NA
Maggie2_f <- fill_missing(Maggie2_n)
study2 <- c("original" = Maggie2, "interpolated" = Maggie2_f)
study2
#> #A single-case data frame with two cases
#> 
#>  original: phase values mt │ interpolated: phase values mt │
#>                A   47.2  1 │                   A   47.2  1 │
#>                A  48.85  2 │                   A  48.85  2 │
#>                A  57.79  3 │                   A  57.79  3 │
#>                A  50.35  4 │                   A  50.35  4 │
#>                A  50.65  5 │                   A  50.65  5 │
#>                B  68.58  6 │                   B  68.58  6 │
#>                B   62.3  7 │                   B   62.3  7 │
#>                B  53.67  8 │                   B  53.67  8 │
#>                B  56.57  9 │                   B  56.57  9 │
#>                B  57.77 10 │                   B  57.77 10 │
#>                B  66.12 11 │                   B  66.12 11 │
#>                B   61.8 12 │                   B   61.8 12 │
#>                B     62 13 │                   B     62 13 │
#>                B  60.55 14 │                   B  60.55 14 │
#>                B  57.22 15 │                   B  57.22 15 │
#> # ... up to five more rows
```

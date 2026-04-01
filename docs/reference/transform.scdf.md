# Transform every single case of a single case data frame

Takes an scdf and applies transformations to each individual case. This
is useful to calculate or modify new variables.

## Usage

``` r
moving_median(x, lag = 1)

moving_mean(x, lag = 1)

local_regression(x, mt = 1:length(x), f = 0.2)

set_na_at(x, first_of, positions = 0)

center_at(x, at = TRUE, shift = 0, part = 0)

first_of(x, positions = 0)

across_cases(...)

all_cases(...)

rowwise(...)

# S3 method for class 'scdf'
transform(`_data`, ...)
```

## Arguments

- x:

  A logical vector.

- lag:

  Number of values surrounding a value to calculate the average

- mt:

  A vector with measurement times.

- f:

  the proportion of surrounding data influencing each data point.

- first_of:

  A logical vector

- positions:

  A numeric vector with relative positions to the first appearance of a
  TRUE value in x.

- at:

  A logical vector. E.g. `phase == "A"`. The first TRUE value of that
  vector is the target position for centring. By default, this is the
  first position.

- shift:

  A value indicating a shift in measurement times for centring. E.g.
  `shift = 4` will centre four measurement-times after the position
  defined by the `at` and `part` arguments.

- part:

  A numeric value between 0 and 1. `0` refers to the first `TRUE` in the
  `at` vector, `1` to the last, and `0.5` to the midpoint of the
  sequence of `TRUE` values. E.g. if you want to centre at the middle of
  phase A, set `at = phase == A, part = 0.5`. Note: decimals are rounded
  to integers.

- ...:

  Expressions.

- \_data:

  An scdf.

## Value

An scdf.

## Details

This function is a method of the generic
[`transform()`](https://rdrr.io/r/base/transform.html) function. Unlike
the generic version, expressions are evaluated **serially**: the result
of one expression is used as the basis for subsequent computations.

Several helper functions can be used inside expressions:

- **`n()`**: returns the number of measurements in a case.

- **`all_cases()`**: extracts the values of a variable across all cases.
  Takes an expression as argument. For example:

  - `mean(all_cases(values))` calculates the mean of `values` across all
    cases.

  - `mean(all_cases(values[phase == "A"]))` calculates the mean of all
    values where `phase == "A"`.

- **`rowwise()`**: applies a calculation separately to each row.
  Example: `rowwise(sum(values, mt, na.rm = TRUE))`.

- **`across_cases()`**: creates new variables or replaces existing ones
  across all cases. Example:
  `across_cases(values_ranked = rank(values, na.last = "keep"))` ranks
  the `values` variable across all cases and creates a new variable
  `values_ranked`.

## See also

Other data manipulation functions:
[`add_l2()`](https://jazznbass.github.io/scan/reference/add_l2.md),
[`as.data.frame.scdf()`](https://jazznbass.github.io/scan/reference/as.data.frame.scdf.md),
[`as_scdf()`](https://jazznbass.github.io/scan/reference/as_scdf.md),
[`batch_apply()`](https://jazznbass.github.io/scan/reference/batch_apply.md),
[`fill_missing()`](https://jazznbass.github.io/scan/reference/fill_missing.md),
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
## Creates a single-case with frequency distributions. The proportion and
## percentage of the frequencies are calculated with transform:
design <- design(
 n = 3,
 level = 5,
 distribution = "binomial",
 n_trials = 20,
 start_value = 0.5
)
study <- random_scdf(design)
transform(study, proportion = values/trials, percentage = proportion * 100)
#> #A single-case data frame with three cases
#> 
#>  [case #1]: phase values mt trials proportion percentage
#>                 A      9  1     20       0.45         45
#>                 A      9  2     20       0.45         45
#>                 A     10  3     20        0.5         50
#>                 A     13  4     20       0.65         65
#>                 A     12  5     20        0.6         60
#>                 B     20  6     20          1        100
#>                 B     20  7     20          1        100
#>                 B     20  8     20          1        100
#>                 B     20  9     20          1        100
#>                 B     20 10     20          1        100
#>                 B     20 11     20          1        100
#>                 B     20 12     20          1        100
#>                 B     20 13     20          1        100
#>                 B     20 14     20          1        100
#>                 B     20 15     20          1        100
#> # ... up to five more rows
#> #  two more cases

## Z standardize the dependent variable and add two new variables:
exampleAB |>
  transform(
    values = scale(values),
    mean_values = mean(values),
    sd_values = sd(values)
  )
#> #A single-case data frame with three cases
#> 
#>  Johanna: values mt phase mean_values sd_values
#>            -1.31  1     A           0         1
#>             -1.4  2     A           0         1
#>            -1.14  3     A           0         1
#>            -0.97  4     A           0         1
#>            -1.48  5     A           0         1
#>            -0.71  6     B           0         1
#>            -0.62  7     B           0         1
#>             0.15  8     B           0         1
#>            -0.28  9     B           0         1
#>            -0.45 10     B           0         1
#>             0.75 11     B           0         1
#>             0.06 12     B           0         1
#>             0.41 13     B           0         1
#>              1.1 14     B           0         1
#>             0.67 15     B           0         1
#> # ... up to five more rows
#> #  two more cases

## Use `all` to calculate global variables.
exampleAB |>
  transform(
    values_center_case = values - mean(values[phase == "A"]),
    values_center_global = values - mean(all(values[phase == "A"])),
    value_dif = values_center_case - values_center_global
  )
#> #A single-case data frame with three cases
#> 
#>  Johanna: values mt phase values_center_case values_center_global value_dif
#>               54  1     A               -0.6                 0.67     -1.27
#>               53  2     A               -1.6                -0.33     -1.27
#>               56  3     A                1.4                 2.67     -1.27
#>               58  4     A                3.4                 4.67     -1.27
#>               52  5     A               -2.6                -1.33     -1.27
#>               61  6     B                6.4                 7.67     -1.27
#>               62  7     B                7.4                 8.67     -1.27
#>               71  8     B               16.4                17.67     -1.27
#>               66  9     B               11.4                12.67     -1.27
#>               64 10     B                9.4                10.67     -1.27
#>               78 11     B               23.4                24.67     -1.27
#>               70 12     B               15.4                16.67     -1.27
#>               74 13     B               19.4                20.67     -1.27
#>               82 14     B               27.4                28.67     -1.27
#>               77 15     B               22.4                23.67     -1.27
#> # ... up to five more rows
#> #  two more cases

## Use `across_cases` to calculate or replace a variable with values from
## all cases. E.g., standardize the dependent variable:
exampleABC |>
  transform(
    across_cases(values = scale(values))
  )
#> #A single-case data frame with three cases
#> 
#>  Marie: values mt phase │ Rosalind: values mt phase │ Lise: values mt phase │
#>          -0.24  1     A │            -1.02  1     A │        -0.81  1     A │
#>          -0.39  2     A │            -1.45  2     A │        -1.16  2     A │
#>           -0.1  3     A │            -1.02  3     A │         0.11  3     A │
#>           0.11  4     A │            -0.67  4     A │         -0.6  4     A │
#>          -0.74  5     A │            -0.53  5     A │         0.32  5     A │
#>          -1.16  6     A │             0.25  6     A │        -0.32  6     A │
#>          -1.23  7     A │            -0.46  7     A │        -1.87  7     A │
#>          -0.17  8     A │            -1.73  8     A │        -1.16  8     A │
#>          -1.16  9     A │            -0.74  9     A │         0.88  9     A │
#>          -1.59 10     A │             -0.1 10     A │         0.11 10     A │
#>           1.52 11     B │             -0.1 11     A │        -1.02 11     A │
#>           0.25 12     B │             0.25 12     A │        -1.16 12     A │
#>            0.6 13     B │            -0.46 13     A │        -1.02 13     A │
#>           1.52 14     B │            -1.09 14     A │         -1.8 14     A │
#>            0.6 15     B │            -0.88 15     A │        -0.74 15     A │
#> # ... up to 15 more rows

## Rank transform the values based on all cases vs. within each case:
exampleABC |>
  transform(
    across_cases(values_across = rank(values, na.last="keep")),
    value_within = rank(values, na.last="keep")
  )
#> #A single-case data frame with three cases
#> 
#>  Marie: values mt phase values_across value_within
#>             58  1     A            40           11
#>             56  2     A          37.5          9.5
#>             60  3     A          44.5           13
#>             63  4     A            50         14.5
#>             51  5     A          23.5          6.5
#>             45  6     A            10          3.5
#>             44  7     A             7            2
#>             59  8     A          41.5           12
#>             45  9     A            10          3.5
#>             39 10     A             5            1
#>             83 11     B            82           27
#>             65 12     B            55           16
#>             70 13     B          65.5         20.5
#>             83 14     B            82           27
#>             70 15     B          65.5         20.5
#> # ... up to 15 more rows
#> #  two more cases

## Three helper functions to smooth the data
Huber2014$Berta |>
transform(
  "compliance (moving median)" = moving_median(compliance),
  "compliance (moving mean)" = moving_mean(compliance),
  "compliance (local regression)" = local_regression(compliance, mt)
)
#> #A single-case data frame with one case
#> 
#>  Berta: compliance mt phase compliance (moving median) compliance (moving mean)
#>                 25  1     A                         25                       25
#>               20.8  2     A                         25                    28.47
#>               39.6  3     A                       39.6                    47.69
#>                 75  4     A                         45                     55.9
#>                 45  5     A                         45                     38.5
#>               14.6  6     A                         45                    32.97
#>               45.8  7     A                         45                    37.36
#>               33.3  8     A                       33.3                    33.99
#>               31.3  9     A                       32.5                     32.6
#>               32.5 10     A                       32.5                     23.1
#>                4.2 11     B                       32.5                    24.37
#>               45.8 12     B                       32.5                    33.82
#>               31.3 13     B                       31.3                    24.84
#>                9.4 14     B                        9.4                    13.51
#>                6.3 15     B                        6.3                        8
#>  compliance (local regression)
#>                          22.02
#>                          28.81
#>                           39.6
#>                           42.3
#>                             45
#>                           45.4
#>                           45.8
#>                          36.07
#>                           32.2
#>                           32.5
#>                          39.15
#>                           45.8
#>                          29.41
#>                          15.05
#>                           6.58
#> # ... up to 14 more rows

## Function first_of() helps to set NAs for specific phases.
## E.g., you want to replace the first two values of phase A and the first
## value of phase B and its preceding value.

byHeart2011 |>
  transform(
    values = set_na_at(values, phase == "A", 0:1),
    values = set_na_at(values, phase == "B", -1:0)
  )
#> #A single-case data frame with 11 cases
#> 
#>  Lisa (Turkish): values mt phase │ Patrick (Spanish): values mt phase │
#>                    <NA>  1     A │                      <NA>  1     A │
#>                    <NA>  2     A │                      <NA>  2     A │
#>                       0  3     A │                         3  3     A │
#>                       0  4     A │                         0  4     A │
#>                    <NA>  5     A │                      <NA>  5     A │
#>                    <NA>  6     B │                      <NA>  6     B │
#>                       5  7     B │                         8  7     B │
#>                       6  8     B │                         8  8     B │
#>                       7  9     B │                         8  9     B │
#>                      10 10     B │                        12 10     B │
#>                      10 11     B │                        13 11     B │
#>                      15 12     B │                        13 12     B │
#>                      16 13     B │                        15 13     B │
#>                      14 14     B │                        14 14     B │
#>                      17 15     B │                        15 15     B │
#> # ... up to 11 more rows
#> #  nine more cases
```

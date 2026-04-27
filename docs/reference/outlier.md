# Handling outliers in single-case data

Identifies and drops outliers within a single-case data frame (scdf).
Outliers can be identified based on mean average deviation (MAD),
standard deviation (SD), confidence intervals (CI), or Cook's Distance
from a Piecewise Linear Regression Model.

## Usage

``` r
# S3 method for class 'sc_outlier'
print(x, digits = "auto", ...)

# S3 method for class 'sc_outlier'
export(object, caption = NA, footnote = NA, filename = NA, ...)

outlier(
  data,
  dvar,
  pvar,
  mvar,
  method = c("MAD", "Cook", "SD", "CI"),
  criteria = 3.5
)
```

## Arguments

- x:

  An object returned by `outlier()`

- digits:

  The minimum number of significant digits to be use. If set to "auto"
  (default), values are predefined.

- ...:

  Further parameters passed to the print function

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

- method:

  Specifies the method for outlier identification. Set `method = "MAD"`
  for mean average deiviation, `method = "SD"` for standard deviations,
  `method = "CI"` for confidence intervals, `method = "Cook"` for Cook's
  Distance based on the Piecewise Linear Regression Model.

- criteria:

  Specifies the criteria for outlier identification. Based on the
  `method` setting.

## Value

|  |  |
|----|----|
|  |  |
| `data` | A single-case data frame with substituted outliers. |
| `dropped.n` | A list with the number of dropped data points for each single-case. |
| `dropped.mt` | A list with the measurement-times of dropped data points for each single-case (values are based on the mt variable of each single-case data frame). |
| `sd.matrix` | A list with a matrix for each case with values for the upper and lower boundaries based on the standard deviation. |
| `ci.matrix` | A list with a matrix for each single-case with values for the upper and lower boundaries based on the confidence interval. |
| `cook` | A list of Cook's Distances for each measurement of each single-case. |
| `criteria` | Criteria used for outlier analysis. |
| `N` | Number of single-cases. |
| `case.names` | Case identifier. |

## Details

For `method = "SD"`, `criteria = 2` would refer t0 two standard
deviations. For `method = "MAD"`, `criteria = 3.5` would refer to 3.5
times the mean average deviation. For `method = "CI"`, `criteria = 0.99`
would refer to a 99 percent confidence interval. For `method = "cook"`,
`criteria = "4/n"` would refer to a Cook's Distance greater than 4/n.

## Functions

- `print(sc_outlier)`: Print results

- `export(sc_outlier)`: Export html results

## See also

Other data manipulation functions:
[`add_l2()`](https://jazznbass.github.io/scan/reference/add_l2.md),
[`as.data.frame.scdf()`](https://jazznbass.github.io/scan/reference/as.data.frame.scdf.md),
[`as_scdf()`](https://jazznbass.github.io/scan/reference/as_scdf.md),
[`batch_apply()`](https://jazznbass.github.io/scan/reference/batch_apply.md),
[`fill_missing()`](https://jazznbass.github.io/scan/reference/fill_missing.md),
[`moving_median()`](https://jazznbass.github.io/scan/reference/transform.scdf.md),
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

## Identify outliers using 1.5 standard deviations as criterion
susanne <- random_scdf(level = 1.0)
res_outlier <- outlier(susanne, method = "SD", criteria = 1.5)
res_outlier
#> Outlier Analysis for Single-Case Data
#> 
#> Case [case #1] : Dropped 2 
#> 

## Identify outliers in the original data from Grosche (2011)
## using Cook's Distance greater than 4/n as criterion
res_outlier <- outlier(Grosche2011, method = "Cook", criteria = "4/n")
res_outlier
#> Outlier Analysis for Single-Case Data
#> 
#> Case Eva : Dropped 1 
#> Case Georg : Dropped 3 
#> Case Olaf : Dropped 2 
#> 
```

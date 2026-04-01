# Add Dummy Variables for Piecewise Linear Models

Adds dummy variables to an scdf for calculating piecewise linear models.

## Usage

``` r
add_dummy_variables(
  scdf,
  model = c("W", "H-M", "B&L-B"),
  contrast_level = c("first", "preceding"),
  contrast_slope = c("first", "preceding")
)
```

## Arguments

- scdf:

  A single-case data frame. See
  [`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md) to
  learn about this format.

- model:

  Model used for calculating the dummy parameters (see Huitema & McKean,
  2000). Default is `model = "W"`. Possible values are: `"B&L-B"`,
  `"H-M"`, `"W"`, and deprecated `"JW"`.

- contrast_level:

  Either "first", "preceding" or a contrast matrix. If NA contrast_level
  is a copy of contrast.

- contrast_slope:

  Either "first", "preceding" or a contrast matrix. If NA contrast_level
  is a copy of contrast.

## Details

This function creates dummy variables for phase levels and phase slopes
according to the specified piecewise regression model. It supports
different contrast coding schemes for both level and slope contrasts.

## Examples

``` r
add_dummy_variables(
 scdf = exampleABC, 
 model = "W", 
 contrast_level = "first", 
 contrast_slope = "first"
)
#> #A single-case data frame with three cases
#> 
#>  Marie: values mt phase phaseB phaseC interB interC
#>             58  0     A      0      0      0      0
#>             56  1     A      0      0      0      0
#>             60  2     A      0      0      0      0
#>             63  3     A      0      0      0      0
#>             51  4     A      0      0      0      0
#>             45  5     A      0      0      0      0
#>             44  6     A      0      0      0      0
#>             59  7     A      0      0      0      0
#>             45  8     A      0      0      0      0
#>             39  9     A      0      0      0      0
#>             83 10     B      1      0      0      0
#>             65 11     B      1      0      1      0
#>             70 12     B      1      0      2      0
#>             83 13     B      1      0      3      0
#>             70 14     B      1      0      4      0
#> # ... up to 15 more rows
#> #  two more cases
```

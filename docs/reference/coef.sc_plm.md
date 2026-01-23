# Extract coefficients from plm/hplm objects

Extract coefficients from plm/hplm objects

## Usage

``` r
# S3 method for class 'sc_plm'
coef(object, ...)
```

## Arguments

- object:

  plm or hplm object

- ...:

  not implemented

## Value

data frame with coefficient table

## Examples

``` r
coefficients(plm(exampleAB$Johanna))
#>              Estimate Std. Error     t value     Pr(>|t|)
#> (Intercept) 54.400000   3.889690 13.98569074 2.173001e-10
#> mt           0.100000   1.587959  0.06297391 9.505673e-01
#> phaseB       7.858333   5.816499  1.35104178 1.954791e-01
#> interB       1.525000   1.616067  0.94364905 3.593813e-01
```

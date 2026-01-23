# Fetches elements from scan objects

The fetch function is a getter function for scan objects returned from
regression functions such as
[`plm()`](https://jazznbass.github.io/scan/reference/plm.md),
[`hplm()`](https://jazznbass.github.io/scan/reference/hplm.md),
[`bplm()`](https://jazznbass.github.io/scan/reference/bplm.md), and
[`mplm()`](https://jazznbass.github.io/scan/reference/mplm.md). It
allows users to extract specific elements from these objects, such as
the fitted model.

## Usage

``` r
fetch(object, what, ...)
```

## Arguments

- object:

  Object returned from a scan function.

- what:

  Element/part to be extracted. Currently, only "model" is supported to
  extract the fitted regression model.

- ...:

  Further parameters passed to the function.

## Value

An object of the respective regression model class.

## See also

Other regression functions:
[`bplm()`](https://jazznbass.github.io/scan/reference/bplm.md),
[`hplm()`](https://jazznbass.github.io/scan/reference/hplm.md),
[`mplm()`](https://jazznbass.github.io/scan/reference/mplm.md),
[`plm()`](https://jazznbass.github.io/scan/reference/plm.md),
[`print.sc_ac()`](https://jazznbass.github.io/scan/reference/autocorr.md),
[`print.sc_bctau()`](https://jazznbass.github.io/scan/reference/corrected_tau.md),
[`trend()`](https://jazznbass.github.io/scan/reference/trend.md)

## Author

Juergen Wilbert

## Examples

``` r
# plm regression
model1 <- plm(example_A24)
fetch(model1, what = "model") |> summary()
#> 
#> Call:
#> glm(formula = formula_full, family = family, data = data, na.action = na.action)
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  258.714     18.036  14.344 1.21e-11 ***
#> year           1.857      5.002   0.371    0.715    
#> phaseB      -150.383     25.694  -5.853 1.23e-05 ***
#> interB        -1.726      5.204  -0.332    0.744    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for gaussian family taken to be 700.6563)
#> 
#>     Null deviance: 111568  on 22  degrees of freedom
#> Residual deviance:  13312  on 19  degrees of freedom
#> AIC: 221.57
#> 
#> Number of Fisher Scoring iterations: 2
#> 
# Multilevel plm regression
model2 <- hplm(exampleAB_50)
fetch(model2, what = "model") |> summary()
#> Linear mixed-effects model fit by maximum likelihood
#>   Data: dat 
#>        AIC      BIC    logLik
#>   8758.802 8790.185 -4373.401
#> 
#> Random effects:
#>  Formula: ~1 | case
#>         (Intercept) Residual
#> StdDev:    9.969762 5.284501
#> 
#> Fixed effects:  values ~ 1 + mt + phaseB + interB 
#>                Value Std.Error   DF  t-value p-value
#> (Intercept) 48.39806 1.4840943 1328 32.61117       0
#> mt           0.57893 0.1156551 1328  5.00566       0
#> phaseB      14.03787 0.6548726 1328 21.43603       0
#> interB       0.90227 0.1189152 1328  7.58755       0
#>  Correlation: 
#>        (Intr) mt     phaseB
#> mt     -0.246              
#> phaseB  0.112 -0.770       
#> interB  0.239 -0.972  0.654
#> 
#> Standardized Within-Group Residuals:
#>         Min          Q1         Med          Q3         Max 
#> -3.25058631 -0.65706678  0.01381196  0.68617339  3.04005252 
#> 
#> Number of Observations: 1381
#> Number of Groups: 50 
# Bayesian plm regression
model3 <- bplm(exampleAB_50, nitt = 5000)
fetch(model3, what = "model") |> summary()
#> 
#>  Iterations = 3001:4991
#>  Thinning interval  = 10
#>  Sample size  = 200 
#> 
#>  DIC: 8574.971 
#> 
#>  G-structure:  ~case
#> 
#>      post.mean l-95% CI u-95% CI eff.samp
#> case     104.3    69.03    143.1    273.2
#> 
#>  R-structure:  ~units
#> 
#>       post.mean l-95% CI u-95% CI eff.samp
#> units     27.97    26.17    29.92      200
#> 
#>  Location effects: values ~ 1 + mt + phaseB + interB 
#> 
#>             post.mean l-95% CI u-95% CI eff.samp  pMCMC   
#> (Intercept)   48.5216  45.0742  51.4028    243.6 <0.005 **
#> mt             0.5846   0.3416   0.8156    200.0 <0.005 **
#> phaseB        14.0314  12.7356  15.1772    200.0 <0.005 **
#> interB         0.8952   0.6623   1.1684    200.0 <0.005 **
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

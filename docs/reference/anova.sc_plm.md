# ANOVA Table for Piecewise Linear Models

Model comparison for piecewise regression models fitted with plm(),
hplm(), or mplm() using likelihood ratio tests.

## Usage

``` r
# S3 method for class 'sc_plm'
anova(object, ...)

# S3 method for class 'sc_hplm'
anova(object, ...)

# S3 method for class 'sc_mplm'
anova(object, ...)
```

## Arguments

- object:

  an object containing the results returned by a plm().

- ...:

  additional plm objects.

## Value

An object of class `anova` containing the results of the model
comparison.

## Details

The function performs likelihood ratio tests to compare nested piecewise
regression models. It extracts the underlying model from the sc_plm,
sc_hplm, or sc_mplm object and passes them to the generic anova()
function for model comparison.

## Examples

``` r
## For glm models with family = "gaussian"
mod1 <- plm(exampleAB$Johanna, level = FALSE, slope = FALSE)
mod2 <- plm(exampleAB$Johanna)
anova(mod1, mod2)
#> Analysis of Deviance Table
#> 
#> Model 1: values ~ 1 + mt
#> Model 2: values ~ 1 + mt + phaseB + interB
#>   Resid. Df Resid. Dev Df Deviance      F Pr(>F)
#> 1        18     449.64                          
#> 2        16     403.46  2   46.183 0.9157 0.4202
## For glm models with family = "poisson"
mod0 <- plm(example_A24, formula = injuries ~ 1, family = "poisson")
mod1 <- plm(example_A24, trend = FALSE, family = "poisson")
anova(mod0, mod1, mod2)
#> Warning: models with response ‘"values"’ removed because response differs from model 1
#> Analysis of Deviance Table
#> 
#> Model 1: injuries ~ 1
#> Model 2: injuries ~ 1 + phaseB + interB
#>   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#> 1        22     643.09                          
#> 2        20      95.78  2   547.31 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## For glm with family = "binomial"
mod0 <- plm(
  exampleAB_score$Christiano, 
  formula = values ~ 1, 
  family = "binomial", 
  var_trials = "trials"
)
mod1 <- plm(
  exampleAB_score$Christiano, 
  trend = FALSE, 
  family = "binomial", 
  var_trials = "trials"
)
anova(mod0, mod1)
#> Analysis of Deviance Table
#> 
#> Model 1: values ~ 1
#> Model 2: values ~ 1 + phaseB + interB
#>   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#> 1        29    267.683                          
#> 2        27     27.124  2   240.56 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## For multilevel models:
mod0 <- hplm(Leidig2018, trend = FALSE, slope = FALSE, level = FALSE)
mod1 <- hplm(Leidig2018, trend = FALSE)
mod2 <- hplm(Leidig2018)
anova(mod0, mod1, mod2)
#>   Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#> 1     1  3 6255.509 6272.876 -3124.754                        
#> 2     2  5 5826.509 5855.454 -2908.254 1 vs 2 433.0002  <.0001
#> 3     3  6 5827.604 5862.339 -2907.802 2 vs 3   0.9044  0.3416
## For mplm
mod0 <- mplm(
  Leidig2018$`1a1`, 
  update = . ~  1, dvar = c("academic_engagement", "disruptive_behavior")
)
mod1 <- mplm(
  Leidig2018$`1a1`, 
  trend = FALSE, 
  dvar = c("academic_engagement", "disruptive_behavior")
)
mod2 <- mplm(
  Leidig2018$`1a1`, 
  dvar = c("academic_engagement", "disruptive_behavior")
)

anova(mod0, mod1, mod2)
#> Analysis of Variance Table
#> 
#> Model 1: y ~ 1
#> Model 2: y ~ 1 + phaseB + interB
#> Model 3: y ~ 1 + mt + phaseB + interB
#>   Res.Df Df Gen.var.  Pillai approx F num Df den Df    Pr(>F)    
#> 1     82     0.52962                                             
#> 2     80 -2  0.44785 0.33250   7.8764      4    158 8.167e-06 ***
#> 3     79 -1  0.44628 0.03166   1.2750      2     78    0.2852    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

# scan - Single-Case Data Analyses for Single and Multiple Baseline Designs

<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/scan?color=blue)](https://CRAN.R-project.org/package=scan)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/scan?color=orange)](https://CRAN.R-project.org/package=scan)
<!-- badges: end -->

A collection of procedures for analysing, visualising, 
and managing single-case data. Multiphase and multibaseline designs are 
supported.

Analysing methods include 

- regression models (multilevel, multivariate, bayesian), 
- between-case standardized-mean difference, 
- overlap indices (including 'PND', 'PEM', 'PAND', 'NAP', 'PET', 'IRD', 'CDC'), 
- Tau-U methods (including meta analysis; baseline corrected tau), and 
- randomization tests. 

Data preparation functions support outlier detection, 
handling missing values, scaling, and custom transformations.   
An export function helps to generate html, word, and latex tables in a 
publication friendly style. A shiny app allows to use scan in a graphical 
userinterface.

More details can be found in the online book 'Analyzing single-case data with 
R and scan', Juergen Wilbert (2026)  
<https://jazznbass.github.io/scan-Book/>.

## Installation

You can install the released version of scan from CRAN with:

``` r
install.packages("scan")
```

Or the development version from GitHub with:

``` r
# install.packages("pak")
pak::pak("jazznbass/scan")
```







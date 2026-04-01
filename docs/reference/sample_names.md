# Samples random names

Generates random names for cases. Names are sampled from a predefined
list of neutral, male, female, or mixed names.

## Usage

``` r
sample_names(n = 1, type = "neutral", seed = NULL)
```

## Arguments

- n:

  Number of names to sample.

- type:

  "neutral", "male", "female", or "mixed" type of names to sample.

- seed:

  A seed for the random number generator. If provided, the sampling will
  be reproducible.

## Value

A character vector with random names.

## Details

This function is useful for anonymizing case names in datasets or for
generating random identifiers.

## Examples

``` r
sample_names(3)
#> [1] "Parris" "Regan"  "Storm" 
```

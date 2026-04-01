# Summary function for an scdf object

Provides a summary of an `scdf` object, including the number of cases,
measurements per case, and design information.

## Usage

``` r
# S3 method for class 'scdf'
summary(object, all_cases = FALSE, ...)
```

## Arguments

- object:

  scdf

- all_cases:

  IF TRUE, more that 10 cases are summarized

- ...:

  not in use

## Details

The summary includes:

- Total number of cases in the `scdf`.

- A table listing each case with the number of measurements and design.

- Variable names with annotations for phase, measurement-time, and
  dependent variable.

- Additional information and author details if available.

## Author

Juergen Wilbert

# Combine single-case data frames into one scdf

Combines several single-case data frames (scdf) into one scdf object.

## Usage

``` r
combine(..., dvar = NULL, pvar = NULL, mvar = NULL, info = NULL, author = NULL)

# S3 method for class 'scdf'
c(...)
```

## Arguments

- ...:

  scdf objects to be combined.

- dvar:

  Character string. Name of the dependent variable. Defaults to the
  dependent variable of the first case provided.

- pvar:

  Character string. Name of the phase variable. Defaults to the phase
  variable of the first case provided.

- mvar:

  Character string. Name of the measurement-time variable. Defaults to
  the measurement-time variable of the first case provided.

- info:

  additional information on the scdf file.

- author:

  author of the data.

## Value

A scdf. If not set differently, the attributes of this scdf are copied
from the first scdf provided (i.e the first argument of the function).

## Author

Juergen Wilbert

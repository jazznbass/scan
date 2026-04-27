# Throws an error

This is a convenience wrapper for internal use.

## Usage

``` r
abort(..., class = NULL, call = FALSE)
```

## Arguments

- ...:

  The message. This can be a character string or a combination of
  character strings and variables.

- class:

  An optional class or vector of classes to add to the error.

- call:

  If TRUE, the call will be included in the error message. Default is
  FALSE.

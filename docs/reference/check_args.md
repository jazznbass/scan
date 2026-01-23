# Check function arguments

This function checks function arguments based on a set of predefined
tests. It is intended for internal use within other functions to
validate their arguments. \#' @param ... A set of expressions defining
the tests to be performed on the function arguments. Each expression
should use one of the predefined test functions available within the
scope of `check_args()`.

## Usage

``` r
check_args(...)
```

## Value

This function does not return a value. If any of the tests fail, it
raises an error with a descriptive message.

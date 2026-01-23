# Print methods for scan objects

Print methods for scan objects

## Usage

``` r
# S3 method for class 'sc_desc'
print(x, digits = "auto", ...)

# S3 method for class 'sc_design'
print(x, ...)

# S3 method for class 'sc_nap'
print(x, digits = "auto", nice = TRUE, complete = FALSE, ...)

# S3 method for class 'sc_overlap'
print(x, digits = "auto", ...)

# S3 method for class 'sc_pem'
print(x, ...)

# S3 method for class 'sc_pet'
print(x, digits = 3, ...)

# S3 method for class 'sc_pnd'
print(x, ...)

# S3 method for class 'sc_power'
print(x, duration = FALSE, digits = 1, ...)

# S3 method for class 'sc_rci'
print(x, digits = 3, ...)

# S3 method for class 'sc_smd'
print(x, digits = "auto", ...)

# S3 method for class 'sc_trend'
print(x, digits = 3, ...)
```

## Arguments

- x:

  Object

- digits:

  The minimum number of significant digits to be use. If set to "auto"
  (default), values are predefined.

- ...:

  Further parameters passed to the print function

- nice:

  If set TRUE (default) output values are rounded and optimized for
  publication tables.

- duration:

  If TRUE the duration for computation is printed.

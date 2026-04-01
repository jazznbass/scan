# Select an scdf case by name

Selects a single case from a scdf object by its name.

## Usage

``` r
# S3 method for class 'scdf'
x$i

# S3 method for class 'scdf'
x[i]
```

## Arguments

- x:

  A scdf object.

- i:

  A case name from x. If i is not a name of x, a warning is issued.

## Value

A scdf object containing only the selected case.

## Author

Juergen Wilbert

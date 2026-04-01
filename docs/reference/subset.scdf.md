# Subset cases, rows, and variables of an scdf

This function is mainly used to filter rows by a logical expression. It
has also arguments to filter variables and cases.

## Usage

``` r
# S3 method for class 'scdf'
subset(x, subset, select, cases, ...)
```

## Arguments

- x:

  An scdf object.

- subset:

  Logical expression indicating rows to keep: missing values are taken
  as false.

- select:

  Expression, indicating columns to select from an scdf.

- cases:

  Expression, indicating cases to keep from an scdf.

- ...:

  not implemented

## Value

An scdf.

## Details

For subsetting rows, a logical expression can be provided in the
`subset` argument. Missing values are treated as FALSE. The expression
is evaluated within each single-case data frame of the scdf.

## Examples

``` r
exampleAB |>
  subset((values < 60 & phase == "A") | (values >= 60 & phase == "B"))
#> #A single-case data frame with three cases
#> 
#>  Johanna: values mt phase │ Karolina: values mt phase │ Anja: values mt phase │
#>               54  1     A │               41  1     A │           55  1     A │
#>               53  2     A │               59  2     A │           58  2     A │
#>               56  3     A │               56  3     A │           53  3     A │
#>               58  4     A │               51  4     A │           50  4     A │
#>               52  5     A │               52  5     A │           52  5     A │
#>               61  6     B │               67  8     B │           68  7     B │
#>               62  7     B │               75  9     B │           68  8     B │
#>               71  8     B │               66 10     B │           81  9     B │
#>               66  9     B │               69 11     B │           67 10     B │
#>               64 10     B │               68 12     B │           78 11     B │
#>               78 11     B │               73 13     B │           73 12     B │
#>               70 12     B │               77 14     B │           72 13     B │
#>               74 13     B │               79 15     B │           78 14     B │
#>               82 14     B │               86 16     B │           81 15     B │
#>               77 15     B │               82 17     B │           78 16     B │
#> # ... up to five more rows
subset(exampleAB_add, select = c(-cigarrets, -depression))
#> #A single-case data frame with one case
#> 
#>  Rolf: day wellbeing phase
#>          1        46  Base
#>          2        49  Base
#>          3        49  Base
#>          4        49  Base
#>          5        50  Base
#>          6        47  Base
#>          7        45  Base
#>          8        59  Base
#>          9        58  Base
#>         10        59  Base
#>         11        59  Base
#>         12        43  Base
#>         13        46  Base
#>         14        52  Base
#>         15        55  Base
#> # ... up to 25 more rows
subset(exampleAB, cases = c(Karolina, Johanna))
#> #A single-case data frame with two cases
#> 
#>  Karolina: values mt phase │ Johanna: values mt phase │
#>                41  1     A │              54  1     A │
#>                59  2     A │              53  2     A │
#>                56  3     A │              56  3     A │
#>                51  4     A │              58  4     A │
#>                52  5     A │              52  5     A │
#>                57  6     B │              61  6     B │
#>                56  7     B │              62  7     B │
#>                67  8     B │              71  8     B │
#>                75  9     B │              66  9     B │
#>                66 10     B │              64 10     B │
#>                69 11     B │              78 11     B │
#>                68 12     B │              70 12     B │
#>                73 13     B │              74 13     B │
#>                77 14     B │              82 14     B │
#>                79 15     B │              77 15     B │
#> # ... up to five more rows
subset(exampleA1B1A2B2, phase %in% c("A1", "B2"), cases = Pawel:Moritz)
#> #A single-case data frame with two cases
#> 
#>  Pawel: values mt phase │ Moritz: values mt phase │
#>             10  1    A1 │             15  1    A1 │
#>             19  2    A1 │             11  2    A1 │
#>             11  3    A1 │             12  3    A1 │
#>              6  4    A1 │             10  4    A1 │
#>             16  5    A1 │             12  5    A1 │
#>             16  6    A1 │             18  6    A1 │
#>             17  7    A1 │             16  7    A1 │
#>             18  8    A1 │             18  8    A1 │
#>             12  9    A1 │             12  9    A1 │
#>             12 10    A1 │             12 10    A1 │
#>             34 31    B2 │             11 11    A1 │
#>             35 32    B2 │             10 12    A1 │
#>             28 33    B2 │             13 13    A1 │
#>             30 34    B2 │              9 14    A1 │
#>             22 35    B2 │             14 15    A1 │
#> # ... up to 10 more rows
```

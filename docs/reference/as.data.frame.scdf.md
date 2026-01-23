# Creating a long format data frame from several single-case data frames (scdf).

The as.data.frame function transposes an scdf into one long data frame.
Additionally, a data frame can be merged that includes level 2 data of
the subjects. This might be helpful to prepare data to be used with
other packages than scan.

## Usage

``` r
# S3 method for class 'scdf'
as.data.frame(x, ..., l2 = NULL, id = "case")
```

## Arguments

- x:

  An scdf object.

- ...:

  Not implemented.

- l2:

  A data frame providing additional variables at Level 2. The scdf has
  to have names for all cases and the Level 2 data frame has to have a
  column with corresponding case names.

- id:

  Variable name of the Level 2 data frame that contains the case names.
  Defaults to "case".

## Value

Returns one data frame with data of all single-cases structured by the
case variable.

## See also

Other data manipulation functions:
[`add_l2()`](https://jazznbass.github.io/scan/reference/add_l2.md),
[`as_scdf()`](https://jazznbass.github.io/scan/reference/as_scdf.md),
[`batch_apply()`](https://jazznbass.github.io/scan/reference/batch_apply.md),
[`fill_missing()`](https://jazznbass.github.io/scan/reference/fill_missing.md),
[`moving_median()`](https://jazznbass.github.io/scan/reference/transform.scdf.md),
[`print.sc_outlier()`](https://jazznbass.github.io/scan/reference/outlier.md),
[`ranks()`](https://jazznbass.github.io/scan/reference/ranks.md),
[`rescale()`](https://jazznbass.github.io/scan/reference/rescale.md),
[`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md),
[`select_cases()`](https://jazznbass.github.io/scan/reference/select_cases.md),
[`set_vars()`](https://jazznbass.github.io/scan/reference/set_vars.md),
[`shift()`](https://jazznbass.github.io/scan/reference/shift.md),
[`smooth_cases()`](https://jazznbass.github.io/scan/reference/smooth_cases.md),
[`standardize()`](https://jazznbass.github.io/scan/reference/standardize.md),
[`truncate_phase()`](https://jazznbass.github.io/scan/reference/truncate_phase.md)

## Author

Juergen Wilbert

## Examples

``` r
## Convert the list of three single-case data frames from Grosche (2011)
### into one long data frame
Grosche2011
#> #A single-case data frame with three cases
#> 
#>  Eva: values mt phase │ Georg: values mt phase │ Olaf: values mt phase │
#>         3.39  0     A │            13  0     A │         7.69  0     A │
#>         2.09  1     A │          17.4  1     A │         7.27  1     A │
#>         1.46  7     A │            13  6     A │         7.02  7     A │
#>         3.23  8     A │          7.74 10     A │         5.37  8     A │
#>         3.06 13     A │          8.82 13     A │         4.57 14     A │
#>         2.83 15     A │           7.5 15     A │         3.02 15     A │
#>         2.44 38     B │          5.82 36     A │         4.68 38     A │
#>         1.86 42     B │          6.26 38     B │         4.97 42     A │
#>         2.17 45     B │           5.6 41     B │         5.73 43     A │
#>         2.59 49     B │         12.79 43     B │         7.63 49     A │
#>         3.92 50     B │          9.17 48     B │         6.98 50     A │
#>         4.14 56     B │         10.59 50     B │          7.5 56     A │
#>         3.31 63     B │          9.23 55     B │         6.91 59     B │
#>         2.69 64     B │         10.71 59     B │         4.86 63     B │
#>         4.27 70     B │          7.61 62     B │         6.71 64     B │
#> # ... up to nine more rows
Grosche2011_long <- as.data.frame(Grosche2011)
Grosche2011_long
#>     case values  mt phase
#> 1    Eva   3.39   0     A
#> 2    Eva   2.09   1     A
#> 3    Eva   1.46   7     A
#> 4    Eva   3.23   8     A
#> 5    Eva   3.06  13     A
#> 6    Eva   2.83  15     A
#> 7    Eva   2.44  38     B
#> 8    Eva   1.86  42     B
#> 9    Eva   2.17  45     B
#> 10   Eva   2.59  49     B
#> 11   Eva   3.92  50     B
#> 12   Eva   4.14  56     B
#> 13   Eva   3.31  63     B
#> 14   Eva   2.69  64     B
#> 15   Eva   4.27  70     B
#> 16   Eva   4.66  77     B
#> 17   Eva   3.20  78     B
#> 18   Eva   4.98  91     B
#> 19   Eva   4.42  92     B
#> 20 Georg  13.00   0     A
#> 21 Georg  17.40   1     A
#> 22 Georg  13.00   6     A
#> 23 Georg   7.74  10     A
#> 24 Georg   8.82  13     A
#> 25 Georg   7.50  15     A
#> 26 Georg   5.82  36     A
#> 27 Georg   6.26  38     B
#> 28 Georg   5.60  41     B
#> 29 Georg  12.79  43     B
#> 30 Georg   9.17  48     B
#> 31 Georg  10.59  50     B
#> 32 Georg   9.23  55     B
#> 33 Georg  10.71  59     B
#> 34 Georg   7.61  62     B
#> 35 Georg   9.78  64     B
#> 36 Georg   8.33  69     B
#> 37 Georg  12.00  71     B
#> 38 Georg  12.19  77     B
#> 39 Georg  10.70  78     B
#> 40 Georg  11.36  85     B
#> 41 Georg   7.89  91     B
#> 42 Georg  10.82  94     B
#> 43 Georg  11.56 101     B
#> 44  Olaf   7.69   0     A
#> 45  Olaf   7.27   1     A
#> 46  Olaf   7.02   7     A
#> 47  Olaf   5.37   8     A
#> 48  Olaf   4.57  14     A
#> 49  Olaf   3.02  15     A
#> 50  Olaf   4.68  38     A
#> 51  Olaf   4.97  42     A
#> 52  Olaf   5.73  43     A
#> 53  Olaf   7.63  49     A
#> 54  Olaf   6.98  50     A
#> 55  Olaf   7.50  56     A
#> 56  Olaf   6.91  59     B
#> 57  Olaf   4.86  63     B
#> 58  Olaf   6.71  64     B
#> 59  Olaf   5.67  70     B
#> 60  Olaf   5.00  71     B
#> 61  Olaf   6.14  80     B
#> 62  Olaf   8.14  91     B
#> 63  Olaf   4.53  97     B

## Combine an scdf with data for l2
Leidig2018_long <- as.data.frame(Leidig2018, l2 = Leidig2018_l2)
names(Leidig2018_long)
#>  [1] "case"                  "academic_engagement"   "mt"                   
#>  [4] "classID"               "weekday"               "disruptive_behavior"  
#>  [7] "phase"                 "class"                 "gender"               
#> [10] "migration"             "first_language_german" "SDQ_TOTAL"            
#> [13] "SDQ_EXTERNALIZING"     "SDQ_INTERNALIZING"     "ITRF_TOTAL"           
#> [16] "ITRF_ACADEMIC"         "ITRF_BEHAVIOR"        
summary(Leidig2018_long)
#>       case      academic_engagement       mt           classID         
#>  1a1    : 108   Min.   :0.0         Min.   :  1.00   Length:3780       
#>  1a2    : 108   1st Qu.:3.0         1st Qu.: 27.75   Class :character  
#>  1a3    : 108   Median :4.0         Median : 54.50   Mode  :character  
#>  1a4    : 108   Mean   :3.7         Mean   : 54.50                     
#>  1a5    : 108   3rd Qu.:5.0         3rd Qu.: 81.25                     
#>  2a1    : 108   Max.   :5.0         Max.   :108.00                     
#>  (Other):3132   NA's   :1366                                           
#>     weekday      disruptive_behavior phase       class          
#>  Min.   :1.000   Min.   :0.0000      A: 730   Length:3780       
#>  1st Qu.:2.000   1st Qu.:0.0000      B:3050   Class :character  
#>  Median :3.000   Median :0.0000               Mode  :character  
#>  Mean   :3.028   Mean   :0.6192                                 
#>  3rd Qu.:4.000   3rd Qu.:1.0000                                 
#>  Max.   :5.000   Max.   :4.0000                                 
#>                  NA's   :1393                                   
#>      gender         migration      first_language_german   SDQ_TOTAL    
#>  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000        Min.   : 4.00  
#>  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000        1st Qu.: 8.00  
#>  Median :0.0000   Median :1.0000   Median :1.0000        Median :11.00  
#>  Mean   :0.1429   Mean   :0.5429   Mean   :0.7429        Mean   :11.46  
#>  3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:1.0000        3rd Qu.:14.00  
#>  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000        Max.   :22.00  
#>                                                                         
#>  SDQ_EXTERNALIZING SDQ_INTERNALIZING   ITRF_TOTAL   ITRF_ACADEMIC   
#>  Min.   : 3.000    Min.   : 0.0      Min.   : 3.0   Min.   : 0.000  
#>  1st Qu.: 6.000    1st Qu.: 1.0      1st Qu.:12.0   1st Qu.: 2.000  
#>  Median : 9.000    Median : 2.0      Median :16.0   Median : 9.000  
#>  Mean   : 8.857    Mean   : 2.6      Mean   :17.4   Mean   : 9.286  
#>  3rd Qu.:11.000    3rd Qu.: 3.0      3rd Qu.:23.0   3rd Qu.:16.000  
#>  Max.   :18.000    Max.   :10.0      Max.   :35.0   Max.   :24.000  
#>                                                                     
#>  ITRF_BEHAVIOR   
#>  Min.   : 0.000  
#>  1st Qu.: 4.000  
#>  Median : 7.000  
#>  Mean   : 8.114  
#>  3rd Qu.:11.000  
#>  Max.   :20.000  
#>                  
```

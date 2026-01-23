# Select and combine phases for overlap analyses

Useful when working with pipe operators. This function allows to select
and combine specific phases from an scdf for overlap analyses. For
example, in an ABAB design one might want to combine both A phases and
both B phases (e.g., `A = c(1, 3)`, `B = c(2, 4)`). The resulting scdf
can then be used in overlap functions such as
[`overlap()`](https://jazznbass.github.io/scan/reference/overlap.md).

## Usage

``` r
select_phases(data, A, B, phase_names = "auto")
```

## Arguments

- data:

  A single-case data frame. See
  [`scdf()`](https://jazznbass.github.io/scan/reference/scdf.md) to
  learn about this format.

- A:

  Selection of the A phase

- B:

  Selection of the B phase

- phase_names:

  A character vector with names for the resulting phases. The default
  `"auto"` generates phase names from the combination of the names of
  the recombined phases.

## Value

An scdf with selected phases

## Details

This function selects and combines the specified phases from the input
scdf and returns the resulting scdf with the selected phases only. This
is particularly useful when working with pipe operators, allowing to
select and combine phases before applying overlap functions. The
resulting scdf contains only the selected and combined phases, with
phase labels "A" and "B". If `phase_names = "auto"`, phase names are
generated from the combination of the names of the recombined phases.
For example, combining phases 1 and 3 will result in the phase name
"1_3". This function simplifies the process of preparing data for
overlap analyses by allowing users to easily select and combine phases
in a single step.

## See also

Other overlap functions:
[`ird()`](https://jazznbass.github.io/scan/reference/ird.md),
[`nap()`](https://jazznbass.github.io/scan/reference/nap.md),
[`overlap()`](https://jazznbass.github.io/scan/reference/overlap.md),
[`pand()`](https://jazznbass.github.io/scan/reference/pand.md),
[`pem()`](https://jazznbass.github.io/scan/reference/pem.md),
[`pet()`](https://jazznbass.github.io/scan/reference/pet.md),
[`pnd()`](https://jazznbass.github.io/scan/reference/pnd.md),
[`print.sc_cdc()`](https://jazznbass.github.io/scan/reference/cdc.md),
[`tau_u()`](https://jazznbass.github.io/scan/reference/tau_u.md)

## Author

Juergen Wilbert

## Examples

``` r
exampleA1B1A2B2_zvt |>
  select_phases(A = c(1, 3), B = c(2, 4)) |>
  overlap()
#> Overlap Indices
#> 
#> Comparing phase 1 against phase 2 
#> 
#>                   Tick     Trick     Track
#> Design       A1A2-B1B2 A1A2-B1B2 A1A2-B1B2
#> PND                 17         0        17
#> PEM                 67        50        50
#> PET                 67        33        33
#> NAP                 68        51        58
#> NAP rescaled        36         3        17
#> PAND                67        50        67
#> IRD               0.33      0.33      0.17
#> Tau_U(A)          0.07     -0.16     -0.04
#> Tau_U(BA)         0.14      0.03     -0.03
#> Base_Tau          0.27     -0.25      0.13
#> Diff_mean         5.50      3.17      0.83
#> Diff_trend       -0.31     -1.10     -0.74
#> SMD               0.52      0.40      0.26
#> Hedges_g          0.56      0.50      0.26
#> 
#> The following variables were used in this analysis:
#> 'zvt' as dependent variable, 'part' as phase variable, and 'day' as measurement-time variable.
```

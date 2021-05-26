# scan 0.53

## Major changes

### New functions

- `is.scdf()`: Tests if an object is of type "scdf" or not.
- `cdc`: Applies the Conservative Dual-Criterion Method (CDC; Swoboda,
Kratochwill, & Levin, 2010) to scdf objects.
``` {.r}
cdc(Beretvas2008)
cdc(exampleAB_decreasing, decreasing = TRUE, trend.method = "bisplit")
```

### Changes in functions

- `subset()`: Argument `subset` changed to `filter`.
- `overlap()`: Added Hedges-g.
- new trend lines added to `plot.scdf()`: Koenig's bi-split / quarter intersect (lines = "trendA_bisplit") and Tukey's tri-split / Wald's slope (lines = "trendA_trisplit").

``` {.r}
plot(exampleAB_50[8], lines = "trendA_bisplit")
plot(example_A24, lines = "trendA_trisplit")
```

- `tau_U()`: Solved bug in meta analysis #6. Reworked the complete function to be more clear and accurate. Added `method_meta` switching between fixed and random-effect meta analyses. Reworked the print function to look nicer.

# scan 0.52

## Major changes

-   `describe()` as the new alias for `describeSC()`
-   `plot.scdf()`, `style_plot()`: New options to style casenames: `names` which takes a list with tag = value structure. Example:

``` {.r}
new_style <- style_plot()
new_style$names$side <- 3
new_style$names$line <- -1.7
new_style$names$col <- "darkred"
new_style$names$cex <- 1.5
new_style$names$at <- 20
new_style$names$adj <- 1
new_style$names$font <- 3
plot(exampleAB_decreasing, style = new_style)
```

-   `plot.scdf()`, `style_plot()`: Different background colors for different phases:

``` {.r}
new_style <- style_plot()
new_style$fill.bg <- c("aliceblue", "mistyrose1", "honeydew")
new_style$lty.seperators <- 0
plot(exampleABC, style = new_style)
```

``` {.r}
plot(exampleABAB, style = c("default", "phase_shade"))
```

# scan 0.50.5

## Major changes

-   fixed bug in plot function

# scan 0.50.4

## Major changes

-   New `subset()` function (method from base `subset()` for selecting variables, rows, and cases. It takes the arguments `subset`, `select`, and `cases`.

``` {.r}
subset(exampleAB, (values < 60 & phase == "A") | (values >= 60 & phase == "B"))
subset(exampleAB_add, select = c(-cigarrets, -depression))
subset(exampleA1B1A2B2, phase %in% c("A1", "B2"), cases = Pawel:Moritz)
```

# scan 0.50.2

## Major changes

-   New `select_cases()` function.

``` {.r}
select_cases(exampleAB, "Johanna", "Karolina")
select_cases(exampleAB, 1,2)
select_cases(exampleAB, "-Johanna")
```

# scan 0.50

## Major changes

-   Started dropping the `SC` extension from function names e.g. `overlapSC()` becomes `overlap()`

see: <https://jazznbass.github.io/scan-Book/the-scan-package.html#changes-with-version-0.50>

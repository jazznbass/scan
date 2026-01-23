# A Shiny app for scan

Run a Shiny app with most of the scan functions.

## Usage

``` r
shinyscan(
  scdf = NULL,
  quiet = TRUE,
  browser = c("external", "viewer"),
  theme = "cerulean",
  ...
)
```

## Arguments

- scdf:

  If you provide an *scdf* here, it will be loaded at startup.

- quiet:

  If TRUE (default) does not report shiny messages in the console.

- browser:

  c("external","viewer")

- theme:

  Bootstrap 5 theme. Default is "cerulean".

- ...:

  Further arguments passed to the
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)
  function.

## Value

This function launches a Shiny application.

## Details

This function launches a shiny application. You need to have `scplot`
and `shiny` installed. These packages are suggested but not necessarily
installed along with scan. `shinyscan()` will ask to install missing
packages.

## Author

Juergen Wilbert

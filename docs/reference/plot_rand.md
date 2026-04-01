# Plot random distribution

This function takes the return of the rand_test function and creates a
histogram with the distribution of the rand sample statistics.

## Usage

``` r
plot_rand(
  object,
  type = "hist",
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  text_observed = "observed",
  color = "lightgrey",
  ...
)
```

## Arguments

- object:

  Object returned from the rand_test() function

- type:

  'hist' or 'xy'.

- xlab:

  Label for the x-axis.

- ylab:

  Label for the y-axis.

- title:

  Plot title.

- text_observed:

  Text for marking the number of observed statistic.

- color:

  Bar color.

- ...:

  Further arguments passed to the plot function.

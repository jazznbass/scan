# Throws a message

Messages can be shown with different levels of detail and as warnings.
This is a convenience wrapper for internal use.

## Usage

``` r
notify(..., type = "!", detail = 1, warning = FALSE)
```

## Arguments

- ...:

  The message. This can be a character string or a combination of
  character strings and variables.

- detail:

  The level of detail for the message. Messages with a detail level
  higher than the current detail level set in the options will not be
  shown. The default is 1.

- warning:

  If TRUE, the message will be shown as a warning. If FALSE (default),
  the message will be shown as an informational message.

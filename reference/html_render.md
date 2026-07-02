# Format and render data frame as HTML table

Format a data frame as an HTML table and render it to the web viewer.

## Usage

``` r
html_render(df, caption = NULL, align = "l", ...)
```

## Arguments

- df:

  A data frame to be rendered as an HTML table.

- caption:

  A string to be displayed above the table.

- align:

  A string indicating the alignment of the cells (default = "l").

- ...:

  Other arguments to pass to `htmlTable`.

## Value

HTML syntax for the `df` table.

## See also

Other table functions:
[`ssm_table()`](http://circumplex.jmgirard.com/reference/ssm_table.md)

#' Create HTML table from SSM results or contrasts
#'
#' Take in the results of an SSM analysis and return an HTML table with the
#' desired formatting.
#'
#' @param ssm_object Required. The results output of `ssm_analyze()`.
#' @param caption A string to be displayed above the table (default = NULL).
#' @param drop_xy A logical indicating whether the x-value and y-value parameters
#'   should be omitted from the output (default = FALSE).
#' @param render A logical indicating whether the table should be displayed in
#'   the RStudio viewer or web browser (default = TRUE).
#' @return A tibble containing the information for the HTML table. As a
#'  side-effect, may also output the HTML table to the web viewer.
#' @family ssm functions
#' @family table functions
#' @export
#' @examples
#' \donttest{
#' # Load example data
#' data("jz2017")
#' 
#' # Create table of profile results
#' res <- ssm_analyze(
#'   jz2017,
#'   scales = 2:9,
#'   measures = c("NARPD", "ASPD")
#' )
#' ssm_table(res)
#' 
#' # Create table of contrast results
#' res <- ssm_analyze(
#'   jz2017,
#'   scales = 2:9,
#'   measures = c("NARPD", "ASPD"), 
#'   contrast = TRUE
#' )
#' ssm_table(res)
#' }
#' 
ssm_table <- function(ssm_object, caption = NULL, 
                      drop_xy = FALSE, render = TRUE) {
  
  stopifnot(class(ssm_object) == "circumplex_ssm")
  stopifnot(is_null_or_char(caption, n = 1))
  stopifnot(is_flag(drop_xy))
  stopifnot(is_flag(render))
  
  df <- ssm_object$results
  
  # Create default caption
  if (is.null(caption)) {
    caption <- dcaption(ssm_object)
  }
  
  # Format output data
  table_df <- 
    data.frame(
      Profile = df$Label,
      Elevation = sprintf("%.2f (%.2f, %.2f)", df$e_est, df$e_lci, df$e_uci),
      `X Value` = sprintf("%.2f (%.2f, %.2f)", df$x_est, df$x_lci, df$x_uci),
      `Y Value` = sprintf("%.2f (%.2f, %.2f)", df$y_est, df$y_lci, df$y_uci),
      Amplitude = sprintf("%.2f (%.2f, %.2f)", df$a_est, df$a_lci, df$a_uci),
      Displacement = sprintf("%.1f (%.1f, %.1f)", df$d_est, df$d_lci, df$d_uci),
      Fit = sprintf("%.3f", df$fit_est)
    )
  
  # Rename first column
  colnames(table_df)[[1]] <- ifelse(
    test = ssm_object$details$contrast, 
    yes = "Contrast", 
    no = "Profile"
  )
  
  # Drop the x and y columns if requested
  if (drop_xy) {
    table_df <- table_df[, -c(3, 4)]
  }
  
  # Format and render HTML table if requested
  if (render) {
    html_render(table_df, caption)
  }
  
  table_df
}

# Build the default caption for the ssm_table function
dcaption <- function(ssm_object) {
  if (ssm_object$details$contrast) {
    sprintf(
      "%s-based Structural Summary Statistic Contrasts with %s CIs",
      ssm_object$details$score_type,
      str_percent(ssm_object$details$interval)
    )
  } else {
    sprintf(
      "%s-based Structural Summary Statistics with %s CIs",
      ssm_object$details$score_type,
      str_percent(ssm_object$details$interval)
    )
  }
}

#' Format and render data frame as HTML table
#'
#' Format a data frame as an HTML table and render it to the web viewer.
#'
#' @param df A data frame to be rendered as an HTML table.
#' @param caption A string to be displayed above the table.
#' @param align A string indicating the alignment of the cells (default = "l").
#' @param ... Other arguments to pass to \code{htmlTable}.
#' @return HTML syntax for the \code{df} table.
#' @family table functions
#' @export
html_render <- function(df, caption = NULL, align = "l", ...) {
  
  stopifnot(is_null_or_char(caption, n = 1))
  stopifnot(align %in% c("l", "c", "r"))
  
  t <- htmlTable::htmlTable(
    df,
    caption = caption,
    align = align,
    align.header = align,
    rnames = FALSE,
    css.cell = "padding-right: 1em; min-width: 3em; white-space: nowrap;",
    ...
  )
  print(t, type = "html")
}

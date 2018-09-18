new_instrument <- function(Scales, Anchors, Items, Norms, Details, ...) {
  new_s3_scalar(
    Scales = Scales,
    Anchors = Anchors,
    Items = Items,
    Norms = Norms,
    Details = Details,
    ...,
    class = "instrument"
  )
}

is_instrument <- function(x) {
  typeof(x) == "list" & class(x) == "instrument"
}

#' @export
print.instrument <- function(x, ...) {
  cat(
    glue(
      "The {x$Details$Name} ({x$Details$Abbrev}) is a {x$Details$Items}-item ",
      "circumplex measure of {x$Details$Construct} from {x$Details$Reference}."
    ),
    "\n"
  ) 
}

#' @export
summary.instrument <- function(object, ...) {
  x <- object
  cat(
    glue(
      "The {x$Details$Name} ({x$Details$Abbrev}) is a {x$Details$Items}-item ",
      "circumplex measure of {x$Details$Construct} from {x$Details$Reference}."
    ),
    "\n"
  )
  cat("\n")
  scales(x)
  cat("\n")
  anchors(x)
  cat("\n")
  items(x)
  cat("\n")
  norms(x)
}

#' Display the scales of a circumplex instrument
#' 
#' Display the scales of a circumplex instrument including the total number of
#' scales and each scale's abbreviation, hypothetical angle, and text label.
#' 
#' @param x Required. An object of the instrument class.
#' @return The same input object. Prints text to console.
#' @family instrument functions
#' @export
#' @examples 
#' data(csip)
#' scales(csip)

scales <- function(x) {
  assert_that(is_instrument(x))
  
  cat(
    glue(
      "The {x$Details$Abbrev} contains {x$Details$Scales} circumplex scales."
    )
  )
  cat("\n")
  for (i in 1:nrow(x$Scales)) {
    xi <- x$Scales[i, ]
    ang <- sprintf("%03d", xi$Angle)
    cat(
      glue("{xi$Abbrev} ({ang} deg): {xi$Label}"),
      "\n"
    )
  }
  
  invisible(x)
}

#' Display the items of a circumplex instrument
#' 
#' Display the items of a circumplex instrument including the total number of
#' items and each item's number and text. The item ordering/numbering displayed
#' here is the same ordering/numbering assumed by the \code{score()} function.
#' 
#' @param x Required. An object of the instrument class.
#' @return The same input object. Prints text to console.
#' @family instrument functions
#' @export
#' @examples 
#' data(csip)
#' items(csip)

items <- function(x) {
  assert_that(is_instrument(x))
  
  cat(
    glue(
      "The {x$Details$Abbrev} contains {x$Details$Items} items.\n"
    )
  )
  cat("\n")
  for (i in 1:nrow(x$Items)) {
    xi <- x$Items[i, ]
    if (!is.na(xi$Number)) {
      cat(glue("{xi$Number}. "))
    }
    cat(glue("{xi$Text}"), "\n")
  }
  
  invisible(x)
}

#' Display the anchors of a circumplex instrument
#' 
#' Display the anchors of a circumplex instrument including the total number of
#' anchors and each anchor's numerical value and text label. Anchors are the
#' response options that respondants select from (e.g., 0 = No, 1 = Yes).
#' 
#' @param x Required. An object of the instrument class.
#' @return The same input object. Prints text to console.
#' @family instrument functions
#' @export
#' @examples 
#' data(csip)
#' anchors(csip)

anchors <- function(x) {
  assert_that(is_instrument(x))
  
  cat(
    glue(
      "The {x$Details$Abbrev} is rated using the following ",
      "{nrow(x$Anchors)}-point scale."
    ),
    "\n"
  )
  for(i in seq_along(x$Anchors$Value)) {
    cat(
      glue(
        "{x$Anchors$Value[[i]]}. {x$Anchors$Label[[i]]}"
      ),
      "\n"
    )
  }
  
  invisible(x)
}

#' Display the norms for a circumplex instrument
#' 
#' Display the norms for a circumplex instrument including the total number of
#' normative data sets available and each data set's number, sample size, 
#' population, and source reference and hyperlink. If another normative data set
#' exists that is not yet included in the package, please let us know.
#' 
#' @param x Required. An object of the instrument class.
#' @return The same input object. Prints text to console.
#' @family instrument functions
#' @export
#' @examples 
#' data(csip)
#' norms(csip)

norms <- function(x) {
  assert_that(is_instrument(x))
  
  samples <- x$Norms[[2]]
  n_norms <- nrow(samples)
  
  if (n_norms == 0) {
    cat("The {x$Details$Abbrev} currently has no normative data sets.\n")
    return()
  }
  
  cat(
    glue(
      "The {x$Details$Abbrev} currently has {n_norms} normative data set(s):"
    ),
    "\n"
  )
  
  for (i in 1:n_norms) {
    sample_i <- samples$Sample[[i]]
    size_i <- samples$Size[[i]]
    pop_i <- samples$Population[[i]]
    cat(
      glue("{sample_i}. {size_i} {pop_i}"),
      "\n  ",
      glue("{samples$Reference[[i]]}"),
      "\n  ",
      glue("<{samples$URL[[i]]}>"),
      "\n"
    )  
  }
  
  invisible(x)
}
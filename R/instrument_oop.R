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
    glue("{x$Details$Abbrev}: {x$Details$Name}"), "\n",
    glue("{x$Details$Items} items, {x$Details$Scales} scales, {nrow(x$Norms[[2]])} normative data sets"), "\n",
    glue("{x$Details$Reference}"), "\n",
    glue("<{x$Details$URL}>"), "\n", sep = ""
  )
}

#' @export
summary.instrument <- function(object, scales = TRUE, anchors = TRUE,
                               items = TRUE, norms = TRUE, ...) {
  x <- object
  print(x)
  if (scales == TRUE) {
    cat("\n")
    scales(x)
  }
  if (anchors == TRUE) {
    cat("\n")
    anchors(x)
  }
  if (items == TRUE) {
    cat("\n")
    items(x)
  }
  if (norms == TRUE) {
    cat("\n")
    norms(x)
  }
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
#' instrument(csip)
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
      "\n", sep = ""
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
#' instrument(csip)
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
    cat(glue("{xi$Text}"), "\n", sep = "")
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
#' instrument(csip)
#' anchors(csip)
anchors <- function(x) {
  assert_that(is_instrument(x))

  cat(
    glue(
      "The {x$Details$Abbrev} is rated using the following ",
      "{nrow(x$Anchors)}-point scale."
    ),
    "\n", sep = ""
  )
  for (i in seq_along(x$Anchors$Value)) {
    cat(
      glue(
        "{x$Anchors$Value[[i]]}. {x$Anchors$Label[[i]]}"
      ),
      "\n", sep = ""
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
#' instrument(csip)
#' norms(csip)
norms <- function(x) {
  assert_that(is_instrument(x))

  samples <- x$Norms[[2]]
  n_norms <- nrow(samples)

  if (n_norms == 0) {
    cat(
      glue("The {x$Details$Abbrev} currently has no normative data sets."),
      "\n", sep = ""
    )
    return()
  }

  cat(
    glue(
      "The {x$Details$Abbrev} currently has {n_norms} normative data set(s):"
    ),
    "\n", sep = ""
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
      "\n", sep = ""
    )
  }

  invisible(x)
}


#' List all available instruments
#'
#' The circumplex package includes information about numerous circumplex
#' instruments including instructions for scoring and standardizing items.
#' Individual instruments can be loaded using the \code{instrument} function.
#'
#' @family instrument functions
#' @export
#' @examples
#' instruments()
instruments <- function() {

  # TODO: Find a way to automate this - maybe dir?

  cat(
    "The circumplex package currently includes 10 instruments:\n",
    "1. CSIE: Circumplex Scales of Interpersonal Efficacy (csie)\n",
    "2. CSIG: Circumplex Scales of Intergroup Goals (csig)\n",
    "3. CSIP: Circumplex Scales of Interpersonal Problems (csip)\n",
    "4. CSIV: Circumplex Scales of Interpersonal Values (csiv)\n",
    "5. IIP-32: Inventory of Interpersonal Problems, Brief Version (iip32)\n",
    "6. IIP-64: Inventory of Interpersonal Problems (iip64)\n",
    "7. IIP-SC: Inventory of Interpersonal Problems, Short Circumplex (iipsc)\n",
    "8. IIS-64: Inventory of Interpersonal Strengths (iis64)\n",
    "9. IPIP-IPC: IPIP Interpersonal Circumplex (ipipipc)\n",
    "10. ISC: Interpersonal Sensitivities Circumplex (isc)\n"
  )
}

#' Load a specific instrument object
#'
#' The circumplex pacakge includes information about numerous circumplex
#' instruments including instructions for scoring and standardizing items to be
#' used in conjunction with the \code{score} and \code{standardize} functions.
#' This function loads the information for a specific instrument into memory.
#' See the \code{instruments} function to list all available instruments.
#'
#' @param code Required. A string (e.g., "iip32") or text in non-standard
#'   evaluation (e.g., iip32). The code of the instrument assigned by this
#'   package and displayed in parentheses by \code{\link{instruments()}}.
#' @return The instrument object for the requested circumplex instrument. If
#'   the function is called without a name assignment (LHS), then the object
#'   will be created in the global environment with the default name as above.
#'   Or, if a name is assigned (LHS), the object will have that name instead.
#' @family instrument functions
#' @export
#' @examples
#' instrument(iip32)
#' instrument("iip32")
#' x <- instrument(iip32)
instrument <- function(code) {
  name_en <- rlang::enquo(code)
  assert_that(is_enquo(name_en))
  name_str <- rlang::quo_name(name_en)
  utils::data(list = name_str)
  invisible(get(name_str))
}

new_instrument <- function(Scales, Anchors, Items, Norms, Details, ...) {
  new_s3_scalar(
    Scales = Scales,
    Anchors = Anchors,
    Items = Items,
    Norms = Norms,
    Details = Details,
    ...,
    class = "circumplex_instrument"
  )
}

is_instrument <- function(x) {
  is.list(x) && inherits(x, "circumplex_instrument")
}

#' @export
print.circumplex_instrument <- function(x, ...) {
  cat(
    x$Details$Abbrev, ": ", x$Details$Name, "\n",
    x$Details$Items, " items, ", x$Details$Scales, " scales, ", 
    nrow(x$Norms[[2]]), " normative data sets", "\n",
    x$Details$Reference, "\n",
    "<", x$Details$URL, ">", "\n",
    sep = ""
  )
}

#' @export
summary.circumplex_instrument <- function(object, scales = TRUE, anchors = TRUE,
                               items = TRUE, norms = TRUE, ...) {
  x <- object
  print(x)
  if (scales) {
    cat("\n")
    scales(x)
  }
  if (anchors) {
    cat("\n")
    anchors(x)
  }
  if (items) {
    cat("\n")
    items(x)
  }
  if (norms) {
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
#' @param items Optional. A logical determining whether the items for each scale
#'   should be displayed below its other information (default = FALSE).
#' @return The same input object. Prints text to console.
#' @family instrument functions
#' @export
#' @examples
#' scales(csip)
#' scales(csip, items = TRUE)
scales <- function(x, items = FALSE) {
  stopifnot(is_instrument(x))
  stopifnot(is_flag(items))

  cat("The ", x$Details$Abbrev, " contains ", x$Details$Scales, 
      " circumplex scales.\n", sep = "")
  for (i in 1:nrow(x$Scales)) {
    xi <- x$Scales[i, ]
    cat(xi$Abbrev, ": ", xi$Label, " (", xi$Angle, " degrees)", "\n", sep = "")
    if (items == TRUE) {
      item_nums <- as.integer(strsplit(xi$Items, ",")[[1]])
      for (j in 1:length(item_nums)) {
        num_j <- item_nums[[j]]
        item_j <- x$Items[[num_j, "Text"]]
        cat("    ", num_j, ". ", item_j, "\n", sep = "")
      }
    }
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
#' items(csip)
items <- function(x) {
  stopifnot(is_instrument(x))

  cat("The ", x$Details$Abbrev, " contains ", x$Details$Items, " items (", 
    x$Details$Status, "):\n",
    ifelse(x$Details$Prefix != "", paste("Prefix: ", x$Details$Prefix, "\n", sep = ""), ""),
    ifelse(x$Details$Suffix != "", paste("Suffix: ", x$Details$Suffix, "\n", sep = ""), ""),
    sep = ""
  )
  for (i in 1:nrow(x$Items)) {
    xi <- x$Items[i, ]
    if (!is.na(xi$Number)) {
      cat(xi$Number, ". ", sep = "")
    }
    cat(xi$Text, "\n", sep = "")
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
#' anchors(csip)
anchors <- function(x) {
  stopifnot(is_instrument(x))

  cat(
    "The ", x$Details$Abbrev, " is rated using the following ",
    nrow(x$Anchors), "-point scale.", "\n", sep = ""
  )
  for (i in seq_along(x$Anchors$Value)) {
    cat(x$Anchors$Value[[i]], ". ", x$Anchors$Label[[i]], "\n",sep = "")
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
#' The population is a short standardized label chosen by this package so that
#' samples can be compared across instruments; it is deliberately broader than
#' the description the original source gives. Several instruments normed on
#' students at a single named university, in a stated period or region, are all
#' labelled "American college students" here. Consult the reference and
#' hyperlink printed alongside it for the source's own description of the
#' sample before treating a normative sample as representative of a
#' population.
#'
#' For most samples the label names the group they were drawn from rather than
#' a frame they were drawn to represent -- but not for all of them, and which
#' is which is recorded per sample in the `Kind` column and printed as the
#' sample's reference kind:
#'
#' \describe{
#'   \item{standardization sample}{The sample was drawn to represent a defined
#'     population, so its mean and standard deviation estimate that
#'     population's. Only the IIP-32 and IIP-64 samples are of this kind.}
#'   \item{identified published source}{The sample's octant statistics are
#'     printed in an identified source -- a study report or an author's norms
#'     page -- and describe that group of people rather than any wider frame.}
#'   \item{no identified source}{The sample's octant statistics appear in no
#'     source that has been identified, whatever is known about the sample
#'     itself, and should be treated as unverified.}
#' }
#'
#' See `vignette("using-instruments")` for what the shipped reference samples
#' are and how to choose among them.
#'
#' @param x Required. An object of the instrument class.
#' @return The same input object. Prints text to console.
#' @family instrument functions
#' @export
#' @examples
#' norms(csip)
norms <- function(x) {
  
  stopifnot(is_instrument(x))

  samples <- x$Norms[[2]]
  n_norms <- nrow(samples)

  cat("The ", x$Details$Abbrev, " currently has ", n_norms, 
      " normative data set(s):", "\n", sep = "")

  for (i in 1:n_norms) {
    sample_i <- samples$Sample[[i]]
    size_i <- samples$Size[[i]]
    pop_i <- samples$Population[[i]]
    # The kind is what a reader choosing between samples most needs and the
    # Population string cannot say: whether these statistics estimate a
    # population, describe a group someone published, or rest on no identified
    # source at all.
    kind_i <- norm_kind_phrase(samples$Kind[[i]])
    cat(
      sample_i, ". ", size_i, " ", pop_i, "\n",
      "Reference kind: ", kind_i, "\n",
      samples$Reference[[i]], "\n",
      "<", samples$URL[[i]], ">", "\n",
      sep = ""
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

  # Enumerate the packaged instruments from the data itself so this listing
  # can never drift from the shipped datasets. Each instrument is a
  # circumplex_instrument object carrying its abbreviation and full name in
  # $Details; example datasets (e.g. aw2009, jz2017, raw) are filtered out by
  # class.
  nms <- utils::data(package = "circumplex")$results[, "Item"]
  insts <- sort(Filter(function(nm) {
    e <- new.env()
    utils::data(list = nm, package = "circumplex", envir = e)
    inherits(get(nm, envir = e), "circumplex_instrument")
  }, nms))

  header <- sprintf(
    "The circumplex package currently includes %d instruments:\n",
    length(insts)
  )
  lines <- vapply(seq_along(insts), function(i) {
    nm <- insts[[i]]
    e <- new.env()
    utils::data(list = nm, package = "circumplex", envir = e)
    d <- get(nm, envir = e)$Details
    sprintf("%d. %s: %s (%s)\n", i, d$Abbrev, d$Name, nm)
  }, character(1))

  cat(c(header, lines))
}

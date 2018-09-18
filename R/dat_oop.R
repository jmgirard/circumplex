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

print.instrument <- function(x, ...) {
  cat(
    glue(
      "The {x$Details$Name} ({x$Details$Abbrev}) is a {x$Details$Items}-item ",
      "circumplex measure of {x$Details$Construct} from {x$Details$Reference}."
    ),
    "\n"
  ) 
}

summary.instrument <- function(x, ...) {
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
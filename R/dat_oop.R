new_instrument <- function(Scales, Items, Norms, Details, ...) {
  new_s3_scalar(
    Scales = Scales,
    Items = Items,
    Norms = Norms,
    Details = Details,
    ...,
    class = "instrument"
  )
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
}

scales <- function(x) {
  # Assert that x has the instrument class
  
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
}

items <- function(x) {
  # Assert that x has the instrument class
  
  cat(
    glue(
      "The {x$Details$Abbrev} contains {x$Details$Items} items.\n"
    )
  )
  cat("\n")
  for (i in 1:nrow(x$Items)) {
    xi <- x$Items[i, ]
    cat(
      glue("{xi$Number}. {xi$Text}"),
      "\n"
    )
  }
}

anchors <- function(x) {
  # Assert that x has the instrument class
  
  cat(
    glue(
      "The {x$Details$Abbrev} is rated using the following ",
      "{x$Details$Max-x$Details$Min+1}-point scale."
    ),
    "\n"
  )
  
  for(i in seq_along(x$Details$Anchors)) {
    cat(
      glue(
        "{i}. {x$Details$Anchors[[i]]}"
      ),
      "\n"
    )
  }
}
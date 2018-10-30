csig_scales <- tibble::tibble(
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  Items = c(
    "8, 16, 24, 32",
    "5, 13, 21, 29",
    "2, 10, 18, 26",
    "7, 15, 23, 31",
    "4, 12, 20, 28",
    "1, 9, 17, 25",
    "6, 14, 22, 30",
    "3, 11, 19, 27"
  ),
  Label = c(
    "Be authoritative",
    "Be tough",
    "Be self-protective",
    "Be wary",
    "Be conflict-avoidant",
    "Be cooperative",
    "Be understanding",
    "Be respected"
  )
)

csig_norms <- tibble::tibble(
  Sample = rep(1, 8),
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  M = c(2.96, 2.53, 2.02, 1.88, 2.24, 2.89, 2.97, 2.96),
  SD = c(0.68, 0.86, 0.88, 0.74, 0.90, 0.76, 0.71, 0.68)
)

csig_norms_src <- tibble::tibble(
  Sample = 1,
  Size = 665,
  Population = "MTurkers from US, Canada, and India about interactions between nations",
  Reference = "Locke (2014)",
  URL = "https://doi.org/10.1177/0146167213514280"
)

csig_anchors <- tibble::tibble(
  Value = 0:4,
  Label = c(
    "It is not at all important that...",
    "It is somewhat important that...",
    "It is moderately important that...",
    "It is very important that...",
    "It is extremely important that..."
  )
)

csig_details <- tibble::tibble(
  Name = "Circumplex Scales of Intergroup Goals",
  Abbrev = "CSIG",
  Items = 32,
  Scales = 8,
  Prefix = "In dealing with other groups, how important is it that we act or appear or are treated this way?",
  Suffix = "",
  Status = "open-access",
  Construct = "interpersonal intergroup goals",
  Reference = "Locke (2014)",
  URL = "https://doi.org/10.1177/0146167213514280"
)

csig_items <- tibble::tribble(
  ~Number, ~Text,
  1, "We are friendly",
  2, "We are the winners in any argument or dispute",
  3, "They respect what we have to say",
  4, "We avoid conflict",
  5, "We show that we can be tough",
  6, "We appreciate what they have to offer",
  7, "We let them fend for themselves",
  8, "We are assertive",
  9, "We celebrate their achievements",
  10, "We do whatever is in our best interest",
  11, "We get the chance to express our views",
  12, "They not get angry with us",
  13, "We not appear vulnerable",
  14, "We understand their point of view",
  15, "They stay out of our business",
  16, "We appear confident",
  17, "They feel we are all on the same team",
  18, "We are better than them",
  19, "They listen to what we have to say",
  20, "We not get into arguments",
  21, "We are aggressive if necessary",
  22, "We show concern for their welfare",
  23, "We not trust them",
  24, "We are decisive",
  25, "We are cooperative",
  26, "We keep our guard up",
  27, "They see us as responsible",
  28, "We not make them angry",
  29, "We not show our weaknesses",
  30, "We are able to compromise",
  31, "We not get entangled in their affairs",
  32, "They see us as capable"
)

csig <- new_instrument(
  Scales = csig_scales,
  Anchors = csig_anchors,
  Items = csig_items,
  Norms = list(csig_norms, csig_norms_src),
  Details = csig_details
)

usethis::use_data(csig, overwrite = TRUE)
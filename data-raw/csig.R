csig_scales <- data.frame(
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

csig_norms <- data.frame(
  Sample = rep(1, 8),
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  M = c(2.96, 2.53, 2.02, 1.88, 2.24, 2.89, 2.97, 2.96),
  SD = c(0.68, 0.86, 0.88, 0.74, 0.90, 0.76, 0.71, 0.68)
)

csig_norms_src <- data.frame(
  Sample = 1,
  Size = 665,
  Population = "MTurkers from US, Canada, and India about interactions between nations",
  Reference = "Locke (2014)",
  URL = "https://doi.org/10.1177/0146167213514280"
)

csig_anchors <- data.frame(
  Value = 0:4,
  Label = c(
    "It is not at all important that...",
    "It is somewhat important that...",
    "It is moderately important that...",
    "It is very important that...",
    "It is extremely important that..."
  )
)

csig_details <- data.frame(
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

csig_items <- data.frame(
  Number = 1:32,
  Text = c(
    "We are friendly",
    "We are the winners in any argument or dispute",
    "They respect what we have to say",
    "We avoid conflict",
    "We show that we can be tough",
    "We appreciate what they have to offer",
    "We let them fend for themselves",
    "We are assertive",
    "We celebrate their achievements",
    "We do whatever is in our best interest",
    "We get the chance to express our views",
    "They not get angry with us",
    "We not appear vulnerable",
    "We understand their point of view",
    "They stay out of our business",
    "We appear confident",
    "They feel we are all on the same team",
    "We are better than them",
    "They listen to what we have to say",
    "We not get into arguments",
    "We are aggressive if necessary",
    "We show concern for their welfare",
    "We not trust them",
    "We are decisive",
    "We are cooperative",
    "We keep our guard up",
    "They see us as responsible",
    "We not make them angry",
    "We not show our weaknesses",
    "We are able to compromise",
    "We not get entangled in their affairs",
    "They see us as capable"
  )
)


csig <- new_instrument(
  Scales = csig_scales,
  Anchors = csig_anchors,
  Items = csig_items,
  Norms = list(csig_norms, csig_norms_src),
  Details = csig_details
)

usethis::use_data(csig, overwrite = TRUE)

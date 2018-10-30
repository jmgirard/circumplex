csie_scales <- tibble::tibble(
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  Items = c(
    "4, 12, 20, 28",
    "7, 15, 23, 31",
    "2, 10, 18, 26",
    "5, 13, 21, 29",
    "8, 16, 24, 32",
    "3, 11, 19, 27",
    "6, 14, 22, 30",
    "1,  9, 17, 25"
  ),
  Label = c(
    "+A",
    "+A-C",
    "-C",
    "-A-C",
    "-A",
    "-A+C",
    "+C",
    "+A+C"
  )
)

csie_norms <- tibble::tibble(
  Sample = rep(1, 8),
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  M = c(7.23, 6.44, 6.93, 7.24, 7.31, 8.51, 7.90, 7.30),
  SD = c(1.68, 1.66, 1.82, 1.54, 1.53, 1.11, 1.20, 1.37)
)

csie_norms_src <- tibble::tibble(
  Sample = 1,
  Size = 367,
  Population = "American college students",
  Reference = "Locke & Sadler (2007)",
  URL = "https://www.webpages.uidaho.edu/klocke/csie.htm"
)

csie_anchors <- tibble::tibble(
  Value = 0:10,
  Label = c(
    "I am not at all confident that",
    "I am not at all confident that",
    "I am mildly confident that",
    "I am mildly confident that",
    "I am moderately confident that",
    "I am moderately confident that",
    "I am moderately confident that",
    "I am very confident that",
    "I am very confident that",
    "I am absolutely confident that",
    "I am absolutely confident that"
  )
)

csie_details <- tibble::tibble(
  Name = "Circumplex Scales of Interpersonal Efficacy",
  Abbrev = "CSIE",
  Items = 32,
  Scales = 8,
  Prefix = "When I am with others, ...",
  Suffix = "",
  Status = "open-access",
  Construct = "interpersonal efficacy",
  Reference = "Locke & Sadler (2007)",
  URL = "https://doi.org/10.1177/0146167206293375"
)

csie_items <- tibble::tribble(
  ~Number, ~Text,
  1, "I can express myself openly",
  2, "I can be tough",
  3, "I can follow the rules",
  4, "I can be assertive",
  5, "I can hide my thoughts and feelings",
  6, "I can fit in",
  7, "I can keep the upper hand",
  8, "I can avoid getting into arguments",
  9, "I can smooth over any difficulties",
  10, "I can be cold and unfriendly when I want to",
  11, "I can get along with them",
  12, "I can speak up when I have something to say",
  13, "I can be submissive",
  14, "I can understand their feelings",
  15, "I can win any arguments or competitions",
  16, "I can be a follower",
  17, "I can get them to listen to what I have to say",
  18, "I can get them to leave me alone",
  19, "I can be nice",
  20, "I can take charge",
  21, "I can disappear into the background when I want",
  22, "I can soothe hurt feelings",
  23, "I can be aggressive if I need to",
  24, "I can avoid making them angry",
  25, "I can be a leader",
  26, "I can be cruel when the situation calls for it",
  27, "I can be giving",
  28, "I can be forceful",
  29, "I can be quiet",
  30, "I can be helpful",
  31, "I can tell them when I am annoyed",
  32, "I can let others take charge"
)

csie <- new_instrument(
  Scales = csie_scales,
  Anchors = csie_anchors,
  Items = csie_items,
  Norms = list(csie_norms, csie_norms_src),
  Details = csie_details
)

usethis::use_data(csie, overwrite = TRUE)
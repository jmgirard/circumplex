csie_scales <- data.frame(
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

csie_norms <- data.frame(
  Sample = rep(1, 8),
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  M = c(7.23, 6.44, 6.93, 7.24, 7.31, 8.51, 7.90, 7.30),
  SD = c(1.68, 1.66, 1.82, 1.54, 1.53, 1.11, 1.20, 1.37)
)

csie_norms_src <- data.frame(
  Sample = 1,
  Size = 367,
  Population = "American college students",
  Reference = "Locke & Sadler (2007)",
  URL = "https://www.webpages.uidaho.edu/klocke/csie.htm"
)

csie_anchors <- data.frame(
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

csie_details <- data.frame(
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

csie_items <- data.frame(
  Number = 1:32,
  Text = c(
    "I can express myself openly",
    "I can be tough",
    "I can follow the rules",
    "I can be assertive",
    "I can hide my thoughts and feelings",
    "I can fit in",
    "I can keep the upper hand",
    "I can avoid getting into arguments",
    "I can smooth over any difficulties",
    "I can be cold and unfriendly when I want to",
    "I can get along with them",
    "I can speak up when I have something to say",
    "I can be submissive",
    "I can understand their feelings",
    "I can win any arguments or competitions",
    "I can be a follower",
    "I can get them to listen to what I have to say",
    "I can get them to leave me alone",
    "I can be nice",
    "I can take charge",
    "I can disappear into the background when I want",
    "I can soothe hurt feelings",
    "I can be aggressive if I need to",
    "I can avoid making them angry",
    "I can be a leader",
    "I can be cruel when the situation calls for it",
    "I can be giving",
    "I can be forceful",
    "I can be quiet",
    "I can be helpful",
    "I can tell them when I am annoyed",
    "I can let others take charge"
  )
)

csie <- new_instrument(
  Scales = csie_scales,
  Anchors = csie_anchors,
  Items = csie_items,
  Norms = list(csie_norms, csie_norms_src),
  Details = csie_details
)

usethis::use_data(csie, overwrite = TRUE)

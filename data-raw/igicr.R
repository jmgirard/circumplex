igicr_scales <- data.frame(
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  Items = c(
    "1,  9, 17, 25",
    "2, 10, 18, 26",
    "3, 11, 19, 27",
    "4, 12, 20, 28",
    "5, 13, 21, 29",
    "6, 14, 22, 30",
    "7, 15, 23, 31",
    "8, 16, 24, 32"
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

igicr_norms <- data.frame(
  Sample = c(rep(1, 8), rep(2, 8), rep(3, 8)),
  Scale = rep(c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"), 3),
  Angle = rep(c(90, 135, 180, 225, 270, 315, 360, 45), 3),
  M = c(
    2.09, 0.97, 1.51, 2.23, 2.38, 2.74, 2.68, 2.35,
    2.13, 1.11, 1.68, 2.24, 2.32, 2.64, 2.52, 2.29,
    2.06, 0.85, 1.37, 2.23, 2.43, 2.82, 2.81, 2.40
  ),
  SD = c(
    0.80, 0.66, 0.85, 0.91, 0.80, 0.76, 0.77, 0.76,
    0.75, 0.68, 0.87, 0.92, 0.79, 0.70, 0.72, 0.73,
    0.83, 0.61, 0.82, 0.91, 0.81, 0.80, 0.78, 0.78
  )
)

igicr_norms_src <- data.frame(
  Sample = c(1, 2, 3),
  Size = c(387, 174, 213),
  Population = c(
    "American community adolescents (age 11-13), overall",
    "American community adolescents (age 11-13), males",
    "American community adolescents (age 11-13), females"
  ),
  Reference = "Trucco, Wright, & Colder (2013)",
  URL = "https://doi.org/10.1177/1073191111411672"
)

igicr_anchors <- data.frame(
  Value = 0:4,
  Label = c(
    "Not at all important to me",
    "Somewhat important to me",
    "Important to me",
    "Very important to me",
    "Extremely important to me"
  )
)

igicr_details <- data.frame(
  Name = "Interpersonal Goals Inventory for Children, Revised Version",
  Abbrev = "IGI-CR",
  Items = 32,
  Scales = 8,
  Prefix = "When with your peers, in general how important is it to you that...",
  Suffix = "",
  Status = "open-access",
  Construct = "interpersonal values",
  Reference = "Trucco, Wright, & Colder (2013)",
  URL = "https://doi.org/10.1177/1073191111411672"
)

igicr_items <- data.frame(
  Number = 1:32,
  Text = c(
    "Your peers respect and admire you",
    "Your peers agree to do what you suggest",
    "You do not show your feelings in front of your peers",
    "You do not do anything ridiculous",
    "Your peers do not get angry with you",
    "Everyone feels good",
    "You feel close to your peers",
    "You say exactly what you want",
    "You appear self-confident and make an impression on your peers",
    "You get to decide what to play",
    "You do not give away too much about yourself",
    "You do not say stupid things when your peers are listening",
    "You do not make your peers angry",
    "You can put your peers in a good mood",
    "Real friendship develops between you",
    "Your peers listen to your opinion",
    "Your peers think you are smart",
    "The group does what you say",
    "You keep your thoughts to yourself",
    "Your peers do not laugh or make fun of you",
    "You do not annoy your peers",
    "You are able to please your peers",
    "Your peers help you when you have a problem",
    "You can state your opinion",
    "You don't back down when there is a disagreement",
    "You feel you have control over your peers",
    "You do not let your peers get too close to you",
    "You do not make a fool of yourself in front of your peers",
    "You let your peers make decisions",
    "You agree with your peers about things",
    "Your peers come to you when they have a problem",
    "You are able to tell your peers how you feel"
  )
)

igicr <- new_instrument(
  Scales = igicr_scales,
  Anchors = igicr_anchors,
  Items = igicr_items,
  Norms = list(igicr_norms, igicr_norms_src),
  Details = igicr_details
)

usethis::use_data(igicr, overwrite = TRUE)

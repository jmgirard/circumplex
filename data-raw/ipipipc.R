ipip_scales <- data.frame(
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  Items = c(
    "6, 14, 22, 30",
    "7, 15, 23, 31",
    "8, 16, 24, 32",
    "1, 9, 17, 25",
    "2, 10, 18, 26",
    "3, 11, 19, 27",
    "4, 12, 20, 28",
    "5, 13, 21, 29"
  ),
  Label = c(
    "Assured-Dominant",
    "Arrogant-Calculating",
    "Cold-Hearted",
    "Aloof-Introverted",
    "Unassured-Submissive",
    "Unassuming-Ingenuous",
    "Warm-Agreeable",
    "Gregarious-Extraverted"
  )
)

ipip_norms <- data.frame(
  Sample = rep(1, 8),
  Scale = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  M = c(2.66, 2.27, 2.46, 2.68, 3.20, 3.64, 4.37, 3.64),
  SD = c(0.71, 0.69, 0.58, 0.79, 0.63, 0.58, 0.47, 0.78)
)

ipip_norms_src <- data.frame(
  Sample = 1,
  Size = 274,
  Population = "American college students",
  Reference = "Markey & Markey (2009)",
  URL = "https://doi.org/10.1177/1073191109340382"
)

ipip_anchors <- data.frame(
  Value = 1:5,
  Label = c(
    "Very Inaccurate",
    "Moderately Inaccurate",
    "Neither Inaccurate nor Accurate",
    "Moderately Accurate",
    "Very Accurate"
  )
)

ipip_details <- data.frame(
  Name = "IPIP Interpersonal Circumplex",
  Abbrev = "IPIP-IPC",
  Items = 32,
  Scales = 8,
  Prefix = "",
  Suffix = "",
  Status = "open-access",
  Construct = "interpersonal traits",
  Reference = "Markey & Markey (2009)",
  URL = "https://doi.org/10.1177/1073191109340382"
)

ipip_items <- data.frame(
  Number = 1:32,
  Text = c(
    "Am quiet around strangers",
    "Speak softly",
    "Tolerate a lot from others",
    "Am interested in people",
    "Feel comfortable around people",
    "Demand to be the center of interest",
    "Cut others to pieces",
    "Believe people should fend for themselves",
    "Am a very private person",
    "Let others finish what they are saying",
    "Take things as they come",
    "Reassure others",
    "Start conversations",
    "Do most of the talking",
    "Contradict others",
    "Don't fall for sob-stories",
    "Don't talk a lot",
    "Seldom toot my own horn",
    "Think of others first",
    "Inquire about others' well-being",
    "Talk to a lot of different people at parties",
    "Speak loudly",
    "Snap at people",
    "Don't put a lot of thought into things",
    "Have little to say",
    "Dislike being the center of attention",
    "Seldom stretch the truth",
    "Get along well with others",
    "Love large parties",
    "Demand attention",
    "Have a sharp tongue",
    "Am not interested in other people's problems"
  )
)

ipipipc <- new_instrument(
  Scales = ipip_scales,
  Anchors = ipip_anchors,
  Items = ipip_items,
  Norms = list(ipip_norms, ipip_norms_src),
  Details = ipip_details
)

usethis::use_data(ipipipc, overwrite = TRUE)

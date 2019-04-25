ipip_scales <- tibble::tibble(
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

ipip_norms <- tibble::tibble(
  Sample = rep(1, 8),
  Scale = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  M = c(2.66, 2.27, 2.46, 2.68, 3.20, 3.64, 4.37, 3.64),
  SD = c(0.71, 0.69, 0.58, 0.79, 0.63, 0.58, 0.47, 0.78)
)

ipip_norms_src <- tibble::tibble(
  Sample = 1,
  Size = 274,
  Population = "American college students",
  Reference = "Markey & Markey (2009)",
  URL = "https://doi.org/10.1177/1073191109340382"
)

ipip_anchors <- tibble::tibble(
  Value = 1:5,
  Label = c(
    "Very Inaccurate",
    "Moderately Inaccurate",
    "Neither Inaccurate nor Accurate",
    "Moderately Accurate",
    "Very Accurate"
  )
)

ipip_details <- tibble::tibble(
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

ipip_items <- tibble::tribble(
  ~Number, ~Text,
  1, "Am quiet around strangers",
  2, "Speak softly",
  3, "Tolerate a lot from others",
  4, "Am interested in people",
  5, "Feel comfortable around people",
  6, "Demand to be the center of interest",
  7, "Cut others to pieces",
  8, "Believe people should fend for themselves",
  9, "Am a very private person",
  10, "Let others finish what they are saying",
  11, "Take things as they come",
  12, "Reassure others",
  13, "Start conversations",
  14, "Do most of the talking",
  15, "Contradict others",
  16, "Don't fall for sob-stories",
  17, "Don't talk a lot",
  18, "Seldom toot my own horn",
  19, "Think of others first",
  20, "Inquire about others' well-being",
  21, "Talk to a lot of different people at parties",
  22, "Speak loudly",
  23, "Snap at people",
  24, "Don't put a lot of thought into things",
  25, "Have little to say",
  26, "Dislike being the center of attention",
  27, "Seldom stretch the truth",
  28, "Get along well with others",
  29, "Love large parties",
  30, "Demand attention",
  31, "Have a sharp tongue",
  32, "Am not interested in other people's problems"
)

ipipipc <- new_instrument(
  Scales = ipip_scales,
  Anchors = ipip_anchors,
  Items = ipip_items,
  Norms = list(ipip_norms, ipip_norms_src),
  Details = ipip_details
)

usethis::use_data(ipipipc, overwrite = TRUE)

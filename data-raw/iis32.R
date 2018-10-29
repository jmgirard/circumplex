iis32_scales <- tibble::tibble(
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  Items = c(
    "14, 16, 18, 20",
    " 3, 10, 19, 25",
    " 6,  8, 26, 28",
    "11, 24, 29, 30",
    " 5, 17, 27, 31",
    " 7, 13, 21, 22",
    " 2,  9, 15, 23",
    " 1,  4, 12, 32"
  ),
  Label = c(
    "Lead",
    "Direct",
    "Balance",
    "Restrain",
    "Cooperate",
    "Consider",
    "Connect",
    "Engage"
  )
)

iis32_norms <- tibble::tibble(
  Sample = rep(1, 8),
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  M = c(4.25, 4.02, 4.26, 4.29, 4.66, 4.65, 4.42, 4.09),
  SD = c(0.99, 0.94, 0.82, 0.88, 0.86, 0.96, 0.87, 0.99)
)

iis32_norms_src <- tibble::tibble(
  Sample = 1,
  Size = 1380,
  Population = "American college students",
  Reference = NA_character_,
  URL = NA_character_
)

iis32_anchors <- tibble::tibble(
  Value = 1:6,
  Label = c(
    "Very little like me",
    "Something like me",
    "Moderately like me",
    "Quite a bit like me",
    "Very like me",
    "Almost always like me"
  )
)

iis32_details <- tibble::tibble(
  Name = "Inventory of Interpersonal Strengths, Brief Version",
  Abbrev = "IIS-32",
  Items = 32,
  Scales = 8,
  Prefix = "",
  Suffix = "",
  Status = "partial text",
  Construct = "interpersonal strengths",
  Reference = "Hatcher & Rogers (2012)",
  URL = "https://doi.org/10.1080/00223891.2012.681818"
)

iis32_items <- tibble::tibble(
  Number = 1:32,
  Text = c(
    "...can really shine...",
    "...enjoy being with...",
    "...make a decision...",
    "...open about myself...",
    "...able to compromise...",
    "...neglecting my own...",
    "...celebrating others' achievements...",
    "...can say 'no'...",
    "...warm with other...",
    "...argue effectively with...",
    "...listen and think...",
    "...connect with others...",
    "...emotional support is...",
    "...be very persuasive...",
    "...be with others...",
    "...strong but fair...",
    "...show my gratitude...",
    "...ask other people...",
    "...needs feel pressing...",
    "...can take charge...",
    "...delighted to help...",
    "...enriched by helping...",
    "...meeting new people...",
    "...others need privacy...",
    "...too much from...",
    "...rely on myself...",
    "...cooperative...",
    "...not being included...",
    "...interested in others...",
    "...resist others' tempting...",
    "...are comfortable giving...",
    "...people at ease..."
  )
)

iis32 <- new_instrument(
  Scales = iis32_scales,
  Anchors = iis32_anchors,
  Items = iis32_items,
  Norms = list(iis32_norms, iis32_norms_src),
  Details = iis32_details
)

usethis::use_data(iis32, overwrite = TRUE)
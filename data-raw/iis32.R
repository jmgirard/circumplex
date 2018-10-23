iis32_scales <- tibble::tibble(
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
  Sample = integer(),
  Abbrev = character(),
  Angle = double(),
  M = double(),
  SD = double()
)

iis32_norms_src <- tibble::tibble(
  Sample = integer(),
  Size = integer(),
  Population = character(),
  Reference = character(),
  URL = character()
)

iis32_anchors <- tibble::tibble(
  Value = 0:5,
  Label = c(
    "Very little like me",
    "Somewhat like me",
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
    "...be very persuasive...",
    "...make a decision...",
    "...neglecting my own...",
    "...listen and think...",
    "...able to compromise...",
    "...celebrating others' achievements...",
    "...enjoy being with...",
    "...can really shine...",
    "...strong but fair...",
    "...argue effectively with...",
    "...can say 'no'...",
    "...others need privacy...",
    "...show my gratitude...",
    "...emotional support is...",
    "...warm with other...",
    "...open about myself...",
    "...ask other people...",
    "...needs feel pressing...",
    "...rely on myself...",
    "...interested in others...",
    "...cooperative...",
    "...delighted to help...",
    "...be with others...",
    "...connect with others...",
    "...can take charge...",
    "...too much from...",
    "...not being included...",
    "...resist others' tempting...",
    "...are comfortable giving...",
    "...enriched by helping...",
    "...meeting new people...",
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
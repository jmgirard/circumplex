iis64_scales <- tibble::tibble(
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  Items = c(
    "11, 16, 22, 29, 31, 37, 40, 58",
    " 7, 10, 20, 28, 39, 46, 55, 59",
    " 4,  5, 15, 18, 36, 48, 50, 53",
    " 3,  8, 21, 34, 45, 51, 54, 56",
    " 2,  9, 14, 32, 41, 47, 49, 60",
    "17, 26, 27, 33, 35, 42, 43, 52",
    " 6, 13, 19, 23, 30, 44, 57, 61",
    " 1, 12, 24, 25, 38, 62, 63, 64"
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

iis64_norms <- tibble::tibble(
  Sample = rep(1, 8),
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  M = c(4.20, 4.10, 4.10, 4.23, 4.59, 4.66, 4.61, 4.16),
  SD = c(1.32, 1.29, 1.34, 1.24, 1.17, 1.14, 1.24, 1.36)
)

iis64_norms_src <- tibble::tibble(
  Sample = 1,
  Size = 684,
  Population = "American college students",
  Reference = "Hatcher & Rogers (2009)",
  URL = "https://doi.org/10.1037/a0017269"
)

iis64_anchors <- tibble::tibble(
  Value = 1:6,
  Label = c(
    "Very little like me",
    "Somewhat like me",
    "Moderately like me",
    "Quite a bit like me",
    "Very like me",
    "Almost always like me"
  )
)

iis64_details <- tibble::tibble(
  Name = "Inventory of Interpersonal Strengths",
  Abbrev = "IIS-64",
  Items = 64,
  Scales = 8,
  Prefix = "",
  Suffix = "",
  Status = "partial text",
  Construct = "interpersonal strengths",
  Reference = "Hatcher & Rogers (2009)",
  URL = "https://doi.org/10.1037/a0017269"
)

iis64_items <- tibble::tibble(
  Number = 1:64,
  Text = c(
    "...can really shine...",
    "...learning from people...",
    "...honest even when...",
    "...hurt my feelings...",
    "...friends with everyone...",
    "...enjoy being with...",
    "...it's not agreeable...",
    "...a constructive solution...",
    "...pretty even-tempered...",
    "...make a decision...",
    "...don't give up...",
    "...open about myself...",
    "...feel good when...",
    "...able to compromise",
    "...neglecting my own...",
    "...in front of...",
    "...celebrating others' achievements...",
    "...can say 'no'...",
    "...warm with other...",
    "...argue effectively with...",
    "...listen and think...",
    "...enjoy lively competition...",
    "...time with people...",
    "...connect with others...",
    "...regain contact with...",
    "...emotional support is...",
    "...they're in trouble...",
    "...my own interests...",
    "...be very persuasive...",
    "...be with others...",
    "...strong but fair...",
    "...show my gratitude...",
    "...makes me happy...",
    "...hesitate to express...",
    "...feelings of gratitude...",
    "...agreements with other...",
    "...ask other people...",
    "...mingling at parties...",
    "...needs feel pressing...",
    "...can take charge...",
    "...as an assistant...",
    "...delighted to help...",
    "...enriched by helping...",
    "...meeting new people...",
    "...others need privacy...",
    "...too much from...",
    "...a good listener...",
    "...rely on myself...",
    "...cooperative...",
    "...not being included...",
    "...interested in others...",
    "...enjoy complimenting others...",
    "...angry without pushing...",
    "...resist others' tempting",
    "...disagreeing with others...",
    "...time to themselves...",
    "...approach other people...",
    "...be assertive with...",
    "...can bounce back...",
    "...are comfortable giving...",
    "...about their lives...",
    "...to loosen up...",
    "...make people laugh...",
    "...people at ease..."
  )
)

iis64 <- new_instrument(
  Scales = iis64_scales,
  Anchors = iis64_anchors,
  Items = iis64_items,
  Norms = list(iis64_norms, iis64_norms_src),
  Details = iis64_details
)

usethis::use_data(iis64, overwrite = TRUE)
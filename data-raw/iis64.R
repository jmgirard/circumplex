iis64_scales <- tibble(
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

iis64_norms <- tibble(
  Sample = rep(1, 8),
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  M = c(4.20, 4.10, 4.10, 4.23, 4.59, 4.66, 4.61, 4.16),
  SD = c(1.32, 1.29, 1.34, 1.24, 1.17, 1.14, 1.24, 1.36)
)

iis64_norms_src <- tibble(
  Sample = 1,
  Size = 684,
  Population = "American college students",
  Reference = "Hatcher & Rogers (2009)",
  URL = "https://doi.org/10.1037/a0017269"
)

iis64_anchors <- tibble(
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

iis64_details <- list(
  Name = "Inventory of Interpersonal Strengths",
  Abbrev = "IIS-64",
  Items = 64,
  Scales = 8,
  Prefix = "",
  Suffix = "",
  Construct = "interpersonal strengths",
  Reference = "Hatcher & Rogers (2009)",
  URL = "https://doi.org/10.1037/a0017269"
)

iis64_items <- tibble(
  Number = NA,
  Text = "Pending permission."
)

iis64 <- new_instrument(
  Scales = iis64_scales,
  Anchors = iis64_anchors,
  Items = iis64_items,
  Norms = list(iis64_norms, iis64_norms_src),
  Details = iis64_details
)
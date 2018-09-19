iip64_scales <- tibble(
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  Items = c(
    "17, 31, 44, 45, 50, 52, 57, 59",
    "1, 22, 24, 29, 32, 40, 56, 64",
    "11, 15, 16, 20, 23, 27, 36, 60",
    "3, 7, 14, 18, 33, 35, 55, 62",
    "5, 6, 8, 9, 12, 13, 19, 39",
    "2, 10, 25, 34, 38, 42, 53, 61",
    "21, 28, 37, 46, 49, 51, 54, 63",
    "4, 26, 30, 41, 43, 47, 48, 58"
  ),
  Label = c(
    "Domineering/Controlling",
    "Vindictive/Self-Centered",
    "Cold/Distant",
    "Socially Inhibited",
    "Nonassertive",
    "Overly Accommodating",
    "Self-Sacrificing",
    "Intrusive/Needy"
  )
)

iip64_norms <- tibble(
  Sample = c(rep(1, 8), rep(2, 8), rep(3, 8)),
  Scale = rep(c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"), 3),
  Angle = rep(c(90, 135, 180, 225, 270, 315, 360, 45), 3),
  M = c(
    4.9, 5.3, 5.7, 6.5, 7.4, 7.8, 8.2, 5.7,
    4.5, 4.8, 5.1, 6.4, 8.0, 8.6, 8.8, 5.4,
    5.3, 5.8, 6.3, 6.6, 6.8, 7.1, 7.7, 5.9
  ) / 8,
  SD = c(
    4.5, 5.1, 5.9, 5.7, 6.1, 5.3, 5.5, 4.8,
    4.1, 4.9, 5.6, 5.7, 6.1, 5.4, 5.5, 4.6,
    4.7, 5.2, 6.1, 5.7, 6.1, 5.1, 5.4, 5.0
  ) / 8
)

iip64_norms_src <- tibble(
  Sample = c(1, 2, 3),
  Size = c(800, 400, 400),
  Population = c(
    "American community adults, overall",
    "American community adults, females",
    "American community adults, males"
  ),
  Reference = "Horowitz, Alden, Wiggins, & Pincus (2003)",
  URL = "https://www.mindgarden.com/113-inventory-of-interpersonal-problems"
)

iip64_anchors <- tibble(
  Value = 0:4,
  Label = c(
    "Not at all",
    "A little bit",
    "Moderately",
    "Quite a bit",
    "Extremely"
  )
)

iip64_details <- list(
  Name = "Inventory of Interpersonal Problems",
  Abbrev = "IIP-64",
  Items = 64,
  Scales = 8,
  Prefix = "",
  Suffix = "",
  Construct = "interpersonal problems",
  Reference = "Horowitz, Alden, Wiggins, & Pincus (2003)",
  URL = "https://www.mindgarden.com/113-inventory-of-interpersonal-problems"
)

iip64_items <- tibble(
  Number = NA,
  Text = "(Mind Garden Inc. has exclusive rights to distribute the IIP-64.)"
)

iip64 <- new_instrument(
  Scales = iip64_scales,
  Anchors = iip64_anchors,
  Items = iip64_items,
  Norms = list(iip64_norms, iip64_norms_src),
  Details = iip64_details
)
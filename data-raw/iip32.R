iip32_scales <- tibble(
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  Items = c(
    "22, 25, 28, 30",
    "14, 16, 17, 18",
    "10, 11, 13, 15",
    "2, 5, 9, 19",
    "4, 6, 7, 12",
    "1, 8, 20, 31",
    "23, 26, 27, 32",
    "3, 21, 24, 29"
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

iip32_norms <- tibble(
  Sample = c(rep(1, 8), rep(2, 8), rep(3, 8)),
  Scale = rep(c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"), 3),
  M = c(
    2.0, 2.7, 2.7, 3.3, 4.0, 4.3, 4.3, 2.7,
    1.8, 2.0, 2.7, 3.0, 4.3, 4.8, 4.7, 2.5,
    2.3, 3.0, 3.0, 3.3, 3.7, 4.0, 3.8, 2.8
  ) / 4,
  SD = c(
    2.5, 3.3, 3.7, 3.3, 3.3, 3.0, 3.3, 2.6,
    2.5, 3.3, 3.3, 3.3, 3.7, 3.3, 3.3, 2.8,
    2.5, 3.3, 3.7, 3.7, 3.3, 3.0, 3.3, 2.8
  ) / 4
)

iip32_norms_src <- tibble(
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

iip32_anchors <- tibble(
  Value = 0:4,
  Label = c(
    "Not at all",
    "A little bit",
    "Moderately",
    "Quite a bit",
    "Extremely"
  )
)

iip32_details <- list(
  Name = "Inventory of Interpersonal Problems, Brief Version",
  Abbrev = "IIP-32",
  Items = 32,
  Scales = 8,
  Prefix = "",
  Suffix = "",
  Construct = "interpersonal problems",
  Reference = "Horowitz, Alden, Wiggins, & Pincus (2003)",
  URL = "https://www.mindgarden.com/113-inventory-of-interpersonal-problems"
)

iip32_items <- tibble(
  Number = NA,
  Text = "(Mind Garden has exclusive rights to distribute this instrument.)",
  Scale = NA
)

iip32 <- new_instrument(
  Scales = iip32_scales,
  Anchors = iip32_anchors,
  Items = iip32_items,
  Norms = list(iip32_norms, iip32_norms_src),
  Details = iip32_details
)
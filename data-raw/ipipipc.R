ipip_scales <- tibble(
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

ipip_norms <- tibble(
  Sample = rep(1, 8),
  Scale = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  M = c(2.66, 2.27, 2.46, 2.68, 3.20, 3.64, 4.37, 3.64),
  SD = c(0.71, 0.69, 0.58, 0.79, 0.63, 0.58, 0.47, 0.78)
)

ipip_norms_src <- tibble(
  Sample = 1,
  Size = 274,
  Population = "American college students",
  Reference = "Markey & Markey (2009)",
  URL = "https://doi.org/10.1177/1073191109340382"
)

ipip_anchors <- tibble(
  Value = 1:5,
  Label = c(
    "Very Inaccurate",
    "Moderately Inaccurate",
    "Neither Inaccurate nor Accurate",
    "Moderately Accurate",
    "Very Accurate"
  )
)

ipip_details <- list(
  Name = "IPIP Interpersonal Circumplex",
  Abbrev = "IPIP-IPC",
  Items = 32,
  Scales = 8,
  Prefix = "",
  Suffix = "",
  Construct = "interpersonal traits",
  Reference = "Markey & Markey (2009)",
  URL = "https://doi.org/10.1177/1073191109340382"
)


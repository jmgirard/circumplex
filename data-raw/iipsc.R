iipsc_scales <- tibble::tibble(
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
    "Domineering",
    "Vindictive",
    "Cold",
    "Socially Avoidant",
    "Nonassertive",
    "Exploitable",
    "Overly Nurturant",
    "Intrusive"
  )
)

iipsc_norms <- tibble::tibble(
  Sample = c(rep(1, 8), rep(2, 8)),
  Scale = rep(c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"), 2),
  Angle = rep(c(90, 135, 180, 225, 270, 315, 360, 45), 2),
  M = c(
    c(3.04, 3.17, 3.60, 4.19, 5.68, 5.54, 5.86, 4.10) / 4,
    c(0.99, 0.97, 1.30, 1.33, 1.81, 1.92, 2.14, 1.43)
  ),
  SD = c(
    c(2.64, 2.76, 3.42, 3.79, 3.66, 3.41, 3.30, 3.20) / 4,
    c(0.82, 0.85, 1.07, 0.98, 0.89, 0.89, 0.90, 1.05)
  )
)

iipsc_norms_src <- tibble::tibble(
  Sample = c(1, 2),
  Size = c(872, 106),
  Population = c(
    "American college students",
    "American psychiatric outpatients"
  ),
  Reference = c(
    "Hopwood, Pincus, DeMoor, & Koonce (2011)",
    "Soldz, Budman, Demby, & Merry (1995)"
  ),
  URL = c(
    "https://doi.org/10.1080/00223890802388665",
    "https://doi.org/10.1177/1073191195002001006"
  )
)

iipsc_anchors <- tibble::tibble(
  Value = 0:4,
  Label = c(
    "Not at all",
    "Somewhat",
    "Moderately",
    "Very",
    "Extremely"
  )
)

iipsc_details <- tibble::tibble(
  Name = "Inventory of Interpersonal Problems Short Circumplex",
  Abbrev = "IIP-SC",
  Items = 32,
  Scales = 8,
  Prefix = "",
  Suffix = "",
  Status = "partial text",
  Construct = "interpersonal problems",
  Reference = "Soldz, Budman, Demby, & Merry (1995)",
  URL = "https://doi.org/10.1177/1073191195002001006"
)

iipsc_items <- tibble::tribble(
  ~Number, ~Text,
  1, "...point of view...",
  2, "...supportive of another...",
  3, "...show affection to...",
  4, "...join in on...",
  5, "...stop bothering me...",
  6, "...I am angry...",
  7, "...my own welfare...",
  8, "...keep things private...",
  9, "...too aggressive toward...",
  10, "...another person's happiness...",
  11, "...feeling of love...",
  12, "...introduce myself to...",
  13, "...confront people with...",
  14, "...assertive without worrying...",
  15, "...please other people...",
  16, "...open up to...",
  17, "...control other people...",
  18, "...too suspicious of...",
  19, "...feel close to...",
  20, "...socialize with other...",
  21, "...assertive with another...",
  22, "...too easily persuaded...",
  23, "...other people's needs...",
  24, "...noticed too much...",
  25, "...argue with other...",
  26, "...revenge against people...",
  27, "...at a distance...",
  28, "...get together socially...",
  29, "...to be firm...",
  30, "...people take advantage...",
  31, "...another person's misery...",
  32, "...tell personal things..."
)

iipsc <- new_instrument(
  Scales = iipsc_scales,
  Anchors = iipsc_anchors,
  Items = iipsc_items,
  Norms = list(iipsc_norms, iipsc_norms_src),
  Details = iipsc_details
)

usethis::use_data(iipsc, overwrite = TRUE)

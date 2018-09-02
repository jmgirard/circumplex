iipsc <- tibble(
  Scale = c(
    "Domineering",
    "Vindictive",
    "Cold",
    "Socially Avoidant",
    "Nonassertive",
    "Exploitable",
    "Overly Nurturant",
    "Intrusive"
  ),
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  M = c(3.04, 3.17, 3.60, 4.19, 5.68, 5.54, 5.86, 4.10) / 4,
  SD = c(2.64, 2.76, 3.42, 3.79, 3.66, 3.41, 3.30, 3.20) / 4,
  Items = c(
    "1,  9, 17, 25",
    "2, 10, 18, 26",
    "3, 11, 19, 27",
    "4, 12, 20, 28",
    "5, 13, 21, 29",
    "6, 14, 22, 30",
    "7, 15, 23, 31",
    "8, 16, 24, 32"
  )
)
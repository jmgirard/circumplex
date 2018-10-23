cais_scales <- tibble::tibble(
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

# TODO: Confirm whether these are based on scale sums or scale means
cais_norms <- tibble::tibble(
  Sample = c(rep(1, 8), rep(2, 8)),
  Scale = rep(c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"), 2),
  Angle = rep(c(90, 135, 180, 225, 270, 315, 360, 45), 2),
  M = c(
    c(3.39, 2.11, 1.85, 1.99, 2.08, 2.76, 3.62, 3.75),
    c(5.19, 3.97, 2.34, 2.76, 3.87, 4.16, 6.52, 6.14)
  ),
  SD = c(
    c(0.84, 0.85, 0.77, 0.74, 0.64, 0.81, 0.86, 0.73),
    c(0.89, 1.08, 0.98, 1.11, 1.12, 0.99, 0.93, 0.87)
  )
)

cais_norms_src <- tibble::tibble(
  Sample = c(1, 2),
  Size = c(213, 194),
  Population = c(
    "American fourth and sixth graders (aged 9 to 13)",
    "American college students (aged 17 to 50)"
  ),
  Reference = c(
    "Sodano & Tracey (2006)",
    "Sodano & Tracey (2006)"
  ),
  URL = c(
    "https://doi.org/10.1207/s15327752jpa8703_12",
    "https://doi.org/10.1207/s15327752jpa8703_12"
  )
)

cais_anchors <- tibble::tibble(
  Value = 1:5,
  Label = c(
    "Never",
    "A little",
    "Some",
    "A lot",
    "Always"
  )
)

cais_details <- tibble::tibble(
  Name = "Child and Adolescent Interpersonal Survey",
  Abbrev = "CAIS",
  Items = 37,
  Scales = 8,
  Prefix = "",
  Suffix = "",
  Status = "open-access",
  Construct = "interpersonal traits",
  Reference = "Sodano & Tracey (2006)",
  URL = "https://doi.org/10.1207/s15327752jpa8703_12"
)

# TODO: Check permission to release item text
cais_items <- tibble::tibble(
  Number = 1:37,
  Text = c(
    "I am tough",
    "I call people names",
    "I hurt people",
    "I am by myself a lot",
    "I am shy",
    "I am calm",
    "I am kind to others",
    "I am fun to be around",
    "I know a lot",
    "I like making trouble",
    "I make people cry",
    "I am alone",
    "I am sad",
    "I am quiet",
    "I try to help others feel better",
    "I am happy",
    "I think I can do a lot",
    "I trick people",
    "I am mean to others",
    "I am hard to get to know",
    "I know very little",
    "Tricking people is mean",
    "I am friendly",
    "I am giving",
    "I speak up for myself",
    "I tell people what to do",
    "I like it when others feel bad",
    "I play by myself",
    "I give in easily",
    "I help people",
    "I play with others",
    "I think I am right",
    "I am sneaky",
    "I am grumpy",
    "I am afraid",
    "I share",
    "I have a lot of friends"
  )
)

cais <- new_instrument(
  Scales = cais_scales,
  Anchors = cais_anchors,
  Items = cais_items,
  Norms = list(cais_norms, cais_norms_src),
  Details = cais_details
)

usethis::use_data(cais, overwrite = TRUE)
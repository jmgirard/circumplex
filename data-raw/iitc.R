iitc_scales <- data.frame(
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  Items = c(
    " 1,  9, 17, 25, 33, 41, 49, 57",
    " 2, 10, 18, 26, 34, 42, 50, 58",
    " 3, 11, 19, 27, 35, 43, 51, 59",
    " 4, 12, 20, 28, 36, 44, 52, 60",
    " 5, 13, 21, 29, 37, 45, 53, 61",
    " 6, 14, 22, 30, 38, 46, 54, 62",
    " 7, 15, 23, 31, 39, 47, 55, 63",
    " 8, 16, 24, 32, 40, 48, 56, 64"
  ),
  Label = c(
    "Dominant",
    "Calculating",
    "Cold",
    "Self-Critical",
    "Submissive",
    "Ingratiating",
    "Warm",
    "Gregarious"
  )
)

iitc_norms <- data.frame(
  Sample = rep(1, 8),
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  M = c(1.18, 0.83, 0.76, 0.85, 1.24, 2.13, 2.66, 1.88),
  SD = c(0.81, 0.77, 0.73, 0.74, 0.78, 0.86, 0.85, 1.02)
)

iitc_norms_src <- data.frame(
  Sample = 1,
  Size = 862,
  Population = "American college students",
  Reference = "Bliton & Pincus (2019)",
  URL = "https://doi.org/10.1177/1073191119864661"
)

iitc_anchors <- data.frame(
  Value = 0:5,
  Label = c(
    "Not at all likely",
    "Slightly likely",
    "Somewhat likely",
    "Quite likely",
    "Very much likely",
    "Extremely likely"
  )
)

iitc_details <- data.frame(
  Name = "Inventory of Influence Tactics Circumplex",
  Abbrev = "IIT-C",
  Items = 64,
  Scales = 8,
  Prefix = "When interacting with others, how likely are you to influence them by...",
  Suffix = "",
  Status = "open-access",
  Construct = "interpersonal influence tactics",
  Reference = "Bliton & Pincus (2019)",
  URL = "https://doi.org/10.1177/1073191119864661"
)

iitc_items <- data.frame(
  Number = 1:64,
  Text = c(
    "Daring them to do it",
    "Reminding them that I am in charge so they do it",
    "Excluding them so they do it",
    "Getting someone else to ask them to do it",
    "Making them feel sorry for me so they do it",
    "Begging them to do it",
    "Suggesting we should do it together",
    "Praising them so they do it",
    "Impressing them to get my way",
    "Reminding them that they owe me so they do it",
    "Blaming them so they do it",
    "Showing them that I am clueless so they do it",
    "Doing nothing until they do it",
    "Crying until they do it",
    "Telling them I'd do anything in return so they do it",
    "Giving them advice so they do it",
    "Using humor to get what I want",
    "Tricking them into doing what I want",
    "Getting angry at them so they do it",
    "Telling them I don't know how to do it so they do it",
    "Waiting for them to do it",
    "Promising to do something in return so they do it",
    "Being patient with them so they do it",
    "Being a role model so they do it",
    "Taking charge of the conversation to get my way",
    "Misleading them into doing what I want",
    "Guilt tripping them until they do it",
    "Making self-critical comments to get what I want",
    "Telling them that they are better at it than I am so they do it",
    "Asking for their help so they do it",
    "Telling them how much I appreciate them doing it",
    "Telling them how exciting it is so they do it",
    "Assigning them the responsibility to do it",
    "Making them doubt themselves to get what I want",
    "Being passive aggressive until they do it",
    "Acting dependent on them so they do it",
    "Pouting to get what I want",
    "Offering a compromise so they do it",
    "Being kind to them so they do it",
    "Being affectionate with them so they do it",
    "Forcing them to do it",
    "Criticizing them so they do it",
    "Ignoring them until they do it",
    "Whining about it so they do it",
    "Sulking so they do it",
    "Telling them how much it means to me so they do it",
    "Showing them how to do it",
    "Becoming enthusiastic about it so they do it",
    "Using my authority to get my way",
    "Lying to get what I want",
    "Exaggerating my problems so they do it",
    "Making them pity me to get what I want",
    "Clinging to them until they do it",
    "Asking politely so they do it",
    "Encouraging them to do it",
    "Flattering them so they do it",
    "Controlling them so they do it",
    "Arguing about it until they do it",
    "Holding a grudge against them until they do it",
    "Putting myself down to get what I want",
    "Hinting at what I want them to do",
    "Telling them how grateful I will be if they do it",
    "Being a good example for them so they do it",
    "Using charm to get my way"
  )
)

iitc <- new_instrument(
  Scales = iitc_scales,
  Anchors = iitc_anchors,
  Items = iitc_items,
  Norms = list(iitc_norms, iitc_norms_src),
  Details = iitc_details
)

usethis::use_data(iitc, overwrite = TRUE)

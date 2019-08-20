iitc_scales <- tibble::tibble(
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

iitc_norms <- tibble::tibble(
  Sample = rep(1, 8),
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  M = c(1.18, 0.83, 0.76, 0.85, 1.24, 2.13, 2.66, 1.88),
  SD = c(0.81, 0.77, 0.73, 0.74, 0.78, 0.86, 0.85, 1.02)
)

iitc_norms_src <- tibble::tibble(
  Sample = 1,
  Size = 862,
  Population = "American college students",
  Reference = "Bliton & Pincus (2019)",
  URL = "https://doi.org/10.1177/1073191119864661"
)

iitc_anchors <- tibble::tribble(
  ~Value, ~Label,
  0, "Not at all likely",
  1, "Slightly likely",
  2, "Somewhat likely",
  3, "Quite likely",
  4, "Very much likely",
  5, "Extremely likely"
)

iitc_details <- tibble::tibble(
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

iitc_items <- tibble::tribble(
  ~Number, ~Text,
  1, "Daring them to do it",
  2, "Reminding them that I am in charge so they do it",
  3, "Excluding them so they do it",
  4, "Getting someone else to ask them to do it",
  5, "Making them feel sorry for me so they do it",
  6, "Begging them to do it",
  7, "Suggesting we should do it together",
  8, "Praising them so they do it",
  9, "Impressing them to get my way",
  10, "Reminding them that they owe me so they do it",
  11, "Blaming them so they do it",
  12, "Showing them that I am clueless so they do it",
  13, "Doing nothing until they do it",
  14, "Crying until they do it",
  15, "Telling them I'd do anything in return so they do it",
  16, "Giving them advice so they do it",
  17, "Using humor to get what I want",
  18, "Tricking them into doing what I want",
  19, "Getting angry at them so they do it",
  20, "Telling them I don't know how to do it so they do it",
  21, "Waiting for them to do it",
  22, "Promising to do something in return so they do it",
  23, "Being patient with them so they do it",
  24, "Being a role model so they do it",
  25, "Taking charge of the conversation to get my way",
  26, "Misleading them into doing what I want",
  27, "Guilt tripping them until they do it",
  28, "Making self-critical comments to get what I want",
  29, "Telling them that they are better at it than I am so they do it",
  30, "Asking for their help so they do it",
  31, "Telling them how much I appreciate them doing it",
  32, "Telling them how exciting it is so they do it",
  33, "Assigning them the responsibility to do it",
  34, "Making them doubt themselves to get what I want",
  35, "Being passive aggressive until they do it",
  36, "Acting dependent on them so they do it",
  37, "Pouting to get what I want",
  38, "Offering a compromise so they do it",
  39, "Being kind to them so they do it",
  40, "Being affectionate with them so they do it",
  41, "Forcing them to do it",
  42, "Criticizing them so they do it",
  43, "Ignoring them until they do it",
  44, "Whining about it so they do it",
  45, "Sulking so they do it",
  46, "Telling them how much it means to me so they do it",
  47, "Showing them how to do it",
  48, "Becoming enthusiastic about it so they do it",
  49, "Using my authority to get my way",
  50, "Lying to get what I want",
  51, "Exaggerating my problems so they do it",
  52, "Making them pity me to get what I want",
  53, "Clinging to them until they do it",
  54, "Asking politely so they do it",
  55, "Encouraging them to do it",
  56, "Flattering them so they do it",
  57, "Controlling them so they do it",
  58, "Arguing about it until they do it",
  59, "Holding a grudge against them until they do it",
  60, "Putting myself down to get what I want",
  61, "Hinting at what I want them to do",
  62, "Telling them how grateful I will be if they do it",
  63, "Being a good example for them so they do it",
  64, "Using charm to get my way"
)

iitc <- new_instrument(
  Scales = iitc_scales,
  Anchors = iitc_anchors,
  Items = iitc_items,
  Norms = list(iitc_norms, iitc_norms_src),
  Details = iitc_details
)

usethis::use_data(iitc, overwrite = TRUE)

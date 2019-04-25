isc_scales <- tibble::tibble(
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  Items = c(
    "1,  9, 17, 25, 33, 41, 49, 57",
    "2, 10, 18, 26, 34, 42, 50, 58",
    "3, 11, 19, 27, 35, 43, 51, 59",
    "4, 12, 20, 28, 36, 44, 52, 60",
    "5, 13, 21, 29, 37, 45, 53, 61",
    "6, 14, 22, 30, 38, 46, 54, 62",
    "7, 15, 23, 31, 39, 47, 55, 63",
    "8, 16, 24, 32, 40, 48, 56, 64"
  ),
  Label = c(
    "Sensitive to Control",
    "Sensitive to Antagonism",
    "Sensitive to Remoteness",
    "Sensitive to Timidity",
    "Sensitive to Passivity",
    "Sensitive to Dependence",
    "Sensitive to Affection",
    "Sensitive to Attention-Seeking"
  )
)

isc_norms <- tibble::tibble(
  Sample = rep(1, 8),
  Scale = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  M = c(6.04, 6.17, 5.23, 4.44, 4.55, 3.91, 3.32, 5.31),
  SD = c(1.06, 1.10, 1.28, 1.16, 1.24, 0.99, 0.91, 1.00)
)

isc_norms_src <- tibble::tibble(
  Sample = 1,
  Size = 649,
  Population = "American college students",
  Reference = "Hopwood et al. (2011)",
  URL = "https://doi.org/10.1111/j.1467-6494.2011.00696.x"
)

isc_anchors <- tibble::tibble(
  Value = 1:8,
  Label = c(
    "Not at all, never bothers me",
    "Very little, rarely bothers me",
    "A little, occasionally bothers me",
    "Slightly, bothers me less than half the time",
    "Somewhat, bothers me more than half of the time",
    "Quite, bothers me frequently",
    "Very much, bothers me most of the time",
    "Extremely, always bothers me"
  )
)

isc_details <- tibble::tibble(
  Name = "Interpersonal Sensitivities Circumplex",
  Abbrev = "ISC",
  Items = 64,
  Scales = 8,
  Prefix = "It bothers me when a person...",
  Suffix = "",
  Status = "open-access",
  Construct = "interpersonal sensitivities",
  Reference = "Hopwood et al. (2011)",
  URL = "https://doi.org/10.1111/j.1467-6494.2011.00696.x"
)

isc_items <- tibble::tribble(
  ~Number, ~Text,
  1, "Thinks they are my boss",
  2, "Disregards my feelings",
  3, "Doesn't show any feelings",
  4, "Won't engage in conversation",
  5, "Cannot assert themselves",
  6, "Avoids conflict at all costs",
  7, "Wants to spend lots of time with me",
  8, "Doesn't respect my privacy",
  9, "Orders me around",
  10, "Doesn't respond to me",
  11, "Doesn't want to be friends",
  12, "Is aloof",
  13, "Doesn't have a backbone",
  14, "Always needs support",
  15, "Tries to get close to me really fast",
  16, "Talks themselves up",
  17, "Talks down to me",
  18, "Is hostile",
  19, "Walls themselves off from me",
  20, "Acts like a wallflower",
  21, "Is weak",
  22, "Always seems to need my help",
  23, "Tells me they love me",
  24, "Shows off",
  25, "Will do anything to get what they want",
  26, "Is deceitful",
  27, "Won't share their feelings with me",
  28, "Won't step up to the plate",
  29, "Allows themselves to be dominated by others",
  30, "Acts helpless",
  31, "Expresses concern about me",
  32, "Talks about themselves",
  33, "Has to call the shots",
  34, "Mistrusts me",
  35, "Is unresponsive",
  36, "Doesn't share their ideas",
  37, "Is always submissive",
  38, "Laughs at all my jokes",
  39, "Acts like we're friends when we don't even know each other",
  40, "Has to be right",
  41, "Intimidates me",
  42, "Doesn't care about my feelings",
  43, "Avoids me",
  44, "Won't participate",
  45, "Cannot make decisions",
  46, "Is dependent on me",
  47, "Humors me",
  48, "Interrupts",
  49, "Is bossy",
  50, "Expects the worst out of me",
  51, "Never gets in touch with me",
  52, "Is really shy",
  53, "Is very passive",
  54, "Believes everything I say",
  55, "Believes I can do no wrong",
  56, "Tries to show me how to do things",
  57, "Always puts themselves first",
  58, "Is mean-spirited",
  59, "Pulls away from me",
  60, "Refuses to lead",
  61, "Is easily controlled",
  62, "Is soft",
  63, "Is clingy",
  64, "Takes control"
)

isc <- new_instrument(
  Scales = isc_scales,
  Anchors = isc_anchors,
  Items = isc_items,
  Norms = list(isc_norms, isc_norms_src),
  Details = isc_details
)

usethis::use_data(isc, overwrite = TRUE)

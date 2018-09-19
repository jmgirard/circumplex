isc_scales <- tibble(
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

isc_norms <- tibble(
  Sample = rep(1, 8),
  Scale = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  M = c(6.04, 6.17, 5.23, 4.44, 4.55, 3.91, 3.32, 5.31),
  SD = c(1.06, 1.10, 1.28, 1.16, 1.24, 0.99, 0.91, 1.00)
)

isc_norms_src <- tibble(
  Sample = 1,
  Size = 649,
  Population = "American college students",
  Reference = "Hopwood et al. (2011)",
  URL = "https://doi.org/10.1111/j.1467-6494.2011.00696.x"
)

isc_anchors <- tibble(
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

isc_details <- list(
  Name = "Interpersonal Sensitivities Circumplex",
  Abbrev = "ISC",
  Items = 64,
  Scales = 8,
  Prefix = "It bothers me when a person...",
  Suffix = "",
  Construct = "interpersonal sensitivities",
  Reference = "Hopwood et al. (2011)",
  URL = "https://doi.org/10.1111/j.1467-6494.2011.00696.x"
)

isc_items <- tibble(
  Number = 1:64,
  Text = c(
    "Thinks they are my boss",
    "Disregards my feelings",
    "Doesn't show any feelings",
    "Won't engage in conversation",
    "Cannot assert themselves",
    "Avoids conflict at all costs",
    "Wants to spend lots of time with me",
    "Doesn't respect my privacy",
    "Orders me around",
    "Doesn't respond to me",
    "Doesn't want to be friends",
    "Is aloof",
    "Doesn't have a backbone",
    "Always needs support",
    "Tries to get close to me really fast",
    "Talks themselves up",
    "Talks down to me",
    "Is hostile",
    "Walls themselves off from me",
    "Acts like a wallflower",
    "Is weak",
    "Always seems to need my help",
    "Tells me they love me",
    "Shows off",
    "Will do anything to get what they want",
    "Is deceitful",
    "Won't share their feelings with me",
    "Won't step up to the plate",
    "Allows themselves to be dominated by others",
    "Acts helpless",
    "Expresses concern about me",
    "Talks about themselves",
    "Has to call the shots",
    "Mistrusts me",
    "Is unresponsive",
    "Doesn't share their ideas",
    "Is always submissive",
    "Laughs at all my jokes",
    "Acts like we're friends when we don't even know each other",
    "Has to be right",
    "Intimidates me",
    "Doesn't care about my feelings",
    "Avoids me",
    "Won't participate",
    "Cannot make decisions",
    "Is dependent on me",
    "Humors me",
    "Interrupts",
    "Is bossy",
    "Expects the worst out of me",
    "Never gets in touch with me",
    "Is really shy",
    "Is very passive",
    "Believes everything I say",
    "Believes I can do no wrong",
    "Tries to show me how to do things",
    "Always puts themselves first",
    "Is mean-spirited",
    "Pulls away from me",
    "Refuses to lead",
    "Is easily controlled",
    "Is soft",
    "Is clingy",
    "Takes control"
  )
)

isc <- new_instrument(
  Scales = isc_scales,
  Anchors = isc_anchors,
  Items = isc_items,
  Norms = list(isc_norms, isc_norms_src),
  Details = isc_details
)
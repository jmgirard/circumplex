iei_scales <- data.frame(
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  Items = c(
    paste(1:8, collapse = ", "),
    paste(9:16, collapse = ", "),
    paste(17:24, collapse = ", "),
    paste(25:32, collapse = ", "),
    paste(33:40, collapse = ", "),
    paste(41:48, collapse = ", "),
    paste(49:56, collapse = ", "),
    paste(57:64, collapse = ", ")
  ),
  Label = c(
    "Confident-Impressive",
    "Superior-Callous",
    "Rejecting-Suspicious",
    "Rejected-Ashamed",
    "Insecure-Anxious",
    "Needy-Empathic",
    "Welcoming-Trusting",
    "Included-Proud"
  )
)

iei_norms <- data.frame(
  Sample = rep(1:2, times = 8),
  Abbrev = rep(c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"), times = 2),
  Angle = rep(c(90, 135, 180, 225, 270, 315, 360, 45), times = 2),
  M = c(2.00, 1.21, 0.91, 1.18, 2.03, 2.63, 2.70, 2.41,
        1.82, 1.22, 1.08, 1.30, 1.83, 2.37, 2.43, 2.20),
  SD = c(.71, .61, .68, .84, .86, .60, .66, .73,
         .79, .53, .66, .89, .90, .51, .68, .79)
)

iei_norms_src <- data.frame(
  Sample = c(1, 2),
  Size = c(1223, 278),
  Population = c("American undergraduate students", "American crowdworkers"),
  Reference = "Horner, Locker, & Hulsey (2024)",
  URL = "https://osf.io/w37dj/"
)

iei_anchors <- data.frame(
  Value = 0:4,
  Label = c(
    "Never feel that way",
    "Seldom feel that way",
    "Sometimes feel that way",
    "Often feel that way",
    "Almost always feel that way"
  )
)

iei_details <- data.frame(
  Name = "Interpersonal Emotion Inventory",
  Abbrev = "IEI",
  Items = 64,
  Scales = 8,
  Prefix = "When I interact with or think about myself in relation to others, I feel...",
  Suffix = "",
  Status = "open-access",
  Constructs = "interpersonal values",
  Reference = "Horner, Locke, & Hulsey (2024)",
  URL = "https://doi.org/10.1080/00223891.2024.2400266"
)

iei_items <- data.frame(
  Number = 1:64,
  Text = c(
    "Admirable",
    "Confident in my strengths",
    "Sure of myself",
    "Self-confident",
    "Attractive",
    "Confident that I am impressive",
    "Like a winner",
    "Unapologetic about winning",
    "Unintimidated",
    "Fully in command",
    "Invincible",
    "Superior",
    "Unsympathetic to suckers",
    "Unconcerned about others' feelings",
    "Impatient with others' shortcomings",
    "Unforgiving",
    "Like I just don't care about others",
    "Hostile",
    "Disapproving of others",
    "Rejecting of others",
    "Like I want to abandon others",
    "Like I want no part of any group",
    "Resentment",
    "Doubtful that I can rely on others",
    "Alienated",
    "Under attack",
    "Distant from them",
    "Rejected",
    "Unwanted",
    "Ashamed of myself",
    "Worthless",
    "Like a loser",
    "Like I am a disappointment",
    "Unsure of myself",
    "Self-doubt",
    "Insecure",
    "Worried that I will be annoying to others",
    "Worried I will disappoint others",
    "Like I need to appease others",
    "Careful not to disappoint others",
    "Self-conscious",
    "That others know better",
    "Anxious to please others",
    "Like I want to console and comfort others",
    "Empathic",
    "Like I want to help others",
    "Accepting of others",
    "Compassionate and caring toward others",
    "Concerned about others' well-being",
    "Admiration for others",
    "Like I really care about others",
    "Gracious toward others",
    "Grateful for others' love and support",
    "Emotionally connected and attuned to others",
    "Trusting in others' kindness",
    "Supported by them",
    "Loving kindness",
    "Close to them",
    "Loved",
    "Welcomed and cared about",
    "Important to others",
    "Valued",
    "Worthy",
    "Proud of myself"
  )
)

iei <- new_instrument(
  Scales = iei_scales,
  Anchors = iei_anchors,
  Items = iei_items,
  Norms = list(iei_norms, iei_norms_src),
  Details = iei_details
)

usethis::use_data(iei, overwrite = TRUE)

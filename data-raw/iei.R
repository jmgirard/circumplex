iei_scales <- tibble::tibble(
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

iei_norms <- tibble::tibble(
  Sample = rep(1:2, times = 8),
  Abbrev = rep(c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"), times = 2),
  Angle = rep(c(90, 135, 180, 225, 270, 315, 360, 45), times = 2),
  M = c(2.00, 1.21, 0.91, 1.18, 2.03, 2.63, 2.70, 2.41,
        1.82, 1.22, 1.08, 1.30, 1.83, 2.37, 2.43, 2.20),
  SD = c(.71, .61, .68, .84, .86, .60, .66, .73,
         .79, .53, .66, .89, .90, .51, .68, .79)
)

iei_norms_src <- tibble::tibble(
  Sample = c(1, 2),
  Size = c(1223, 278),
  Population = c("American undergraduate students", "American crowdworkers"),
  Reference = "Horner, Locker, & Hulsey (2024)",
  URL = "https://osf.io/w37dj/"
)

iei_anchors <- tibble::tibble(
  Value = 0:4,
  Label = c(
    "Never feel that way",
    "Seldom feel that way",
    "Sometimes feel that way",
    "Often feel that way",
    "Almost always feel that way"
  )
)

iei_details <- tibble::tibble(
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

iei_items <- tibble::tribble(
  ~Number, ~Text,
  1, "Admirable",
  2, "Confident in my strengths",
  3, "Sure of myself",
  4, "Self-confident",
  5, "Attractive",
  6, "Confident that I am impressive",
  7, "Like a winner",
  8, "Unapologetic about winning",
  9, "Unintimidated",
  10, "Fully in command",
  11, "Invincible",
  12, "Superior",
  13, "Unsympathetic to suckers",
  14, "Unconcerned about others' feelings",
  15, "Impatient with others' shortcomings",
  16, "Unforgiving",
  17, "Like I just don't care about others",
  18, "Hostile",
  19, "Disapproving of others",
  20, "Rejecting of others",
  21, "Like I want to abandon others",
  22, "Like I want no part of any group",
  23, "Resentment",
  24, "Doubtful that I can rely on others",
  25, "Alienated",
  26, "Under attack",
  27, "Distant from them",
  28, "Rejected",
  29, "Unwanted",
  30, "Ashamed of myself",
  31, "Worthless",
  32, "Like a loser",
  33, "Like I am a disappointment",
  34, "Unsure of myself",
  35, "Self-doubt",
  36, "Insecure",
  37, "Worried that I will be annoying to others",
  38, "Worried I will disappoint others",
  39, "Like I need to appease others",
  40, "Careful not to disappoint others",
  41, "Self-conscious",
  42, "That others know better",
  43, "Anxious to please others",
  44, "Like I want to console and comfort others",
  45, "Empathic",
  46, "Like I want to help others",
  47, "Accepting of others",
  48, "Compassionate and caring toward others",
  49, "Concerned about others' well-being",
  50, "Admiration for others",
  51, "Like I really care about others",
  52, "Gracious toward others",
  53, "Grateful for others' love and support",
  54, "Emotionally connected and attuned to others",
  55, "Trusting in others' kindness",
  56, "Supported by them",
  57, "Loving kindness",
  58, "Close to them",
  59, "Loved",
  60, "Welcomed and cared about",
  61, "Important to others",
  62, "Valued",
  63, "Worthy",
  64, "Proud of myself"
)

iei <- new_instrument(
  Scales = iei_scales,
  Anchors = iei_anchors,
  Items = iei_items,
  Norms = list(iei_norms, iei_norms_src),
  Details = iei_details
)

usethis::use_data(iei, overwrite = TRUE)

csiv_scales <- tibble::tibble(
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  Items = c(
    "1,  9, 17, 25, 33, 41, 49, 57",
    "4, 12, 20, 28, 36, 44, 52, 60",
    "7, 15, 23, 31, 39, 47, 55, 63",
    "2, 10, 18, 26, 34, 42, 50, 58",
    "5, 13, 21, 29, 37, 45, 53, 61",
    "8, 16, 24, 32, 40, 48, 56, 64",
    "3, 11, 19, 27, 35, 43, 51, 59",
    "6, 14, 22, 30, 38, 46, 54, 62"
  ),
  Label = c(
    "+A",
    "+A-C",
    "-C",
    "-A-C",
    "-A",
    "-A+C",
    "+C",
    "+A+C"
  )
)

csiv_norms <- tibble::tibble(
  Sample = rep(1, 8),
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  M = c(2.53, 1.38, 1.10, 1.66, 1.77, 2.67, 2.83, 2.93),
  SD = c(0.63, 0.71, 0.70, 0.78, 0.75, 0.71, 0.69, 0.57)
)

csiv_norms_src <- tibble::tibble(
  Sample = 1,
  Size = 1200,
  Population = "American college students",
  Reference = "Locke (2000)",
  URL = "https://www.webpages.uidaho.edu/klocke/csiv.htm"
)

csiv_anchors <- tibble::tibble(
  Value = 0:4,
  Label = c(
    "Not important to me",
    "Mildly important to me",
    "Moderately important to me",
    "Very important to me",
    "Extremely important to me"
  )
)

csiv_details <- tibble::tibble(
  Name = "Circumplex Scales of Interpersonal Values",
  Abbrev = "CSIV",
  Items = 64,
  Scales = 8,
  Prefix = "When I am with him/her/them, it is...",
  Suffix = "",
  Status = "open-access",
  Constructs = "interpersonal values",
  Reference = "Locke (2000)",
  URL = "https://doi.org/10.1207/S15327752JPA7502_6"
)

csiv_items <- tibble::tribble(
  ~Number, ~Text,
  1, "That I appear confident",
  2, "That I not reveal my positive feelings for them",
  3, "That I feel connected to them",
  4, "That I appear forceful",
  5, "That I conform to their expectations",
  6, "That I am unique",
  7, "That I keep my guard up",
  8, "That I put their needs before mine",
  9, "That they acknowledge when I am right",
  10, "That I not make a social blunder",
  11, "That they show interest in what I have to say",
  12, "That I attack back when I am attacked",
  13, "That I not get into an argument",
  14, "That they not deceive me",
  15, "That they not know what I am thinking or feelings",
  16, "That they not see me as getting in their way",
  17, "That I get the chance to voice my views",
  18, "That I appear aloof",
  19, "That they support me when I am having problems",
  20, "That I keep the upper hand",
  21, "That I do what they want me to do",
  22, "That I express myself openly",
  23, "That I not show I care about them",
  24, "That I get along with them",
  25, "That they respect my privacy",
  26, "That I not make mistakes in front of them",
  27, "That they understand me",
  28, "That I put my needs first",
  29, "That I live up to their expectations",
  30, "That they respect what I have to say",
  31, "That they keep their distance from me",
  32, "That they not reject me",
  33, "That I not back down when disagreements arise",
  34, "That I not say something stupid",
  35, "That they come to me with their problems",
  36, "That I am the one in charge",
  37, "That I not make them angry",
  38, "That I have an impact on them",
  39, "That I do better than them",
  40, "That I make them feel happy",
  41, "That they not tell me what to do",
  42, "That I not expose myself to the possibility of rejection",
  43, "That they are considerate",
  44, "That I avenge insults and injustices against me",
  45, "That I go along with what they want to do",
  46, "That they show me respect",
  47, "That they see me as cool and unemotional",
  48, "That they approve of me",
  49, "That I am obeyed when I am in authority",
  50, "That I not expose myself to ridicule",
  51, "That they stay with me when things aren't going well",
  52, "That I win if there is an argument",
  53, "That I not embarrass myself",
  54, "That they see me as responsible",
  55, "That I appear detached",
  56, "That they think I am a nice person",
  57, "That they admit it when they are wrong",
  58, "That I keep my thoughts or feelings to myself",
  59, "That they show concern for how I am feeling",
  60, "That they mind their own business",
  61, "That they not get angry with me",
  62, "That they listen to what I have to say",
  63, "That I not reveal what I am really like",
  64, "That they not get their feelings hurt"
)

csiv <- new_instrument(
  Scales = csiv_scales,
  Anchors = csiv_anchors,
  Items = csiv_items,
  Norms = list(csiv_norms, csiv_norms_src),
  Details = csiv_details
)

usethis::use_data(csiv, overwrite = TRUE)
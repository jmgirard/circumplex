csiv_scales <- data.frame(
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

csiv_norms <- data.frame(
  Sample = rep(1, 8),
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  M = c(2.53, 1.38, 1.10, 1.66, 1.77, 2.67, 2.83, 2.93),
  SD = c(0.63, 0.71, 0.70, 0.78, 0.75, 0.71, 0.69, 0.57)
)

csiv_norms_src <- data.frame(
  Sample = 1,
  Size = 1200,
  Population = "American college students",
  Reference = "Locke (2000)",
  URL = "https://www.webpages.uidaho.edu/klocke/csiv.htm"
)

csiv_anchors <- data.frame(
  Value = 0:4,
  Label = c(
    "Not important to me",
    "Mildly important to me",
    "Moderately important to me",
    "Very important to me",
    "Extremely important to me"
  )
)

csiv_details <- data.frame(
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

csiv_items <- data.frame(
  Number = 1:64,
  Text = c(
    "That I appear confident",
    "That I not reveal my positive feelings for them",
    "That I feel connected to them",
    "That I appear forceful",
    "That I conform to their expectations",
    "That I am unique",
    "That I keep my guard up",
    "That I put their needs before mine",
    "That they acknowledge when I am right",
    "That I not make a social blunder",
    "That they show interest in what I have to say",
    "That I attack back when I am attacked",
    "That I not get into an argument",
    "That they not deceive me",
    "That they not know what I am thinking or feelings",
    "That they not see me as getting in their way",
    "That I get the chance to voice my views",
    "That I appear aloof",
    "That they support me when I am having problems",
    "That I keep the upper hand",
    "That I do what they want me to do",
    "That I express myself openly",
    "That I not show I care about them",
    "That I get along with them",
    "That they respect my privacy",
    "That I not make mistakes in front of them",
    "That they understand me",
    "That I put my needs first",
    "That I live up to their expectations",
    "That they respect what I have to say",
    "That they keep their distance from me",
    "That they not reject me",
    "That I not back down when disagreements arise",
    "That I not say something stupid",
    "That they come to me with their problems",
    "That I am the one in charge",
    "That I not make them angry",
    "That I have an impact on them",
    "That I do better than them",
    "That I make them feel happy",
    "That they not tell me what to do",
    "That I not expose myself to the possibility of rejection",
    "That they are considerate",
    "That I avenge insults and injustices against me",
    "That I go along with what they want to do",
    "That they show me respect",
    "That they see me as cool and unemotional",
    "That they approve of me",
    "That I am obeyed when I am in authority",
    "That I not expose myself to ridicule",
    "That they stay with me when things aren't going well",
    "That I win if there is an argument",
    "That I not embarrass myself",
    "That they see me as responsible",
    "That I appear detached",
    "That they think I am a nice person",
    "That they admit it when they are wrong",
    "That I keep my thoughts or feelings to myself",
    "That they show concern for how I am feeling",
    "That they mind their own business",
    "That they not get angry with me",
    "That they listen to what I have to say",
    "That I not reveal what I am really like",
    "That they not get their feelings hurt"
  )
)

csiv <- new_instrument(
  Scales = csiv_scales,
  Anchors = csiv_anchors,
  Items = csiv_items,
  Norms = list(csiv_norms, csiv_norms_src),
  Details = csiv_details
)

usethis::use_data(csiv, overwrite = TRUE)

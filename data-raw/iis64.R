iis64_scales <- tibble::tibble(
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  Items = c(
    "11, 16, 22, 29, 31, 37, 40, 58",
    " 7, 10, 20, 28, 39, 46, 55, 59",
    " 4,  5, 15, 18, 36, 48, 50, 53",
    " 3,  8, 21, 34, 45, 51, 54, 56",
    " 2,  9, 14, 32, 41, 47, 49, 60",
    "17, 26, 27, 33, 35, 42, 43, 52",
    " 6, 13, 19, 23, 30, 44, 57, 61",
    " 1, 12, 24, 25, 38, 62, 63, 64"
  ),
  Label = c(
    "Lead",
    "Direct",
    "Balance",
    "Restrain",
    "Cooperate",
    "Consider",
    "Connect",
    "Engage"
  )
)

iis64_norms <- tibble::tibble(
  Sample = rep(1, 8),
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  M = c(4.20, 4.10, 4.10, 4.23, 4.59, 4.66, 4.61, 4.16),
  SD = c(1.32, 1.29, 1.34, 1.24, 1.17, 1.14, 1.24, 1.36)
)

iis64_norms_src <- tibble::tibble(
  Sample = 1,
  Size = 684,
  Population = "American college students",
  Reference = "Hatcher & Rogers (2009)",
  URL = "https://doi.org/10.1037/a0017269"
)

iis64_anchors <- tibble::tibble(
  Value = 1:6,
  Label = c(
    "Very little like me",
    "Something like me",
    "Moderately like me",
    "Quite a bit like me",
    "Very like me",
    "Almost always like me"
  )
)

iis64_details <- tibble::tibble(
  Name = "Inventory of Interpersonal Strengths",
  Abbrev = "IIS-64",
  Items = 64,
  Scales = 8,
  Prefix = "",
  Suffix = "",
  Status = "partial text",
  Construct = "interpersonal strengths",
  Reference = "Hatcher & Rogers (2009)",
  URL = "https://doi.org/10.1037/a0017269"
)

iis64_items <- tibble::tribble(
  ~Number, ~Text,
  1, "I can really shine in the spotlight",
  2, "I enjoy learning from people who have more experience than I do",
  3, "It's important to me to be honest even when it's difficult",
  4, "I recover quickly when people hurt my feelings",
  5, "I realize ",
  6, "I enjoy being with other people",
  7, "I can assert my needs even when it's not agreeable with others",
  8, "When someone irritates me, I look for a constructive solution",
  9, "I'm pretty even-tempered with others",
  10, "I can make a decision even if others disagree",
  11, "I don't give up easily in competitive situations",
  12, "I feel comfortable being open about myself",
  13, "I feel good when I'm with other people",
  14, "I am able to compromise",
  15, "I can help others with their needs without neglecting my own",
  16, "I feel confident in front of other people",
  17, "I enjoy celebrating others' achievements",
  18, "I can say 'no' to others",
  19, "I'm warm with other people",
  20, "I argue effectively with others",
  21, "I can listen and think before I act in relationships",
  22, "I enjoy lively competition with others",
  23, "I look forward to spending time with people",
  24, "I put myself out there in order to connect with others",
  25, "I try to regain contact with people with whom I've lost touch",
  26, "Offering other people emotional support is important to me",
  27, "I stick by my friends when they're in trouble",
  28, "I know how to look after my own interests",
  29, "I can be very persuasive",
  30, "I make time to be with others",
  31, "I am a strong but fair leader",
  32, "I show my gratitude for what others do for me",
  33, "It makes me happy when others are happy",
  34, "I hesitate to express opinions about others without all the facts",
  35, "My feelings of gratitude warm my relationships with others",
  36, "I like to be clear on my agreements with other people",
  37, "I can ask other people for what I want",
  38, "I enjoy mingling at parties",
  39, "I can take care of myself, even when others' needs feel pressing",
  40, "I can take charge in a group",
  41, "I work really well as an assistant",
  42, "When friends as for favors, I'm delighted to help them out",
  43, "I feel enriched by helping others",
  44, "I'm excited about meeting new people",
  45, "I recognize when others need privacy",
  46, "I can let other people know when I think that they're asking for too much from me",
  47, "I'm a good listener",
  48, "I can rely on myself when I'm having problems with others",
  49, "I'm cooperative",
  50, "I'm ok with not being included in all activities",
  51, "I can be interested in others without being nosy",
  52, "I enjoy complimenting others",
  53, "I know how to be angry without pushing people away",
  54, "I can resist others' tempting me to indulge myself",
  55, "I'm comfortable disagreeing with others",
  56, "I'm respectful of others' need for time to themselves",
  57, "I approach other people with friendliness",
  58, "I am able to be assertive with other people",
  59, "When others get me down, I can bounce back",
  60, "I don't ask others for more than they are comfortable giving",
  61, "I like asking people about their lives",
  62, "I try to help people to loosen up",
  63, "I can make people laugh",
  64, "I put other people at ease"
)

iis64 <- new_instrument(
  Scales = iis64_scales,
  Anchors = iis64_anchors,
  Items = iis64_items,
  Norms = list(iis64_norms, iis64_norms_src),
  Details = iis64_details
)

usethis::use_data(iis64, overwrite = TRUE)
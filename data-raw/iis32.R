iis32_scales <- tibble::tibble(
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  Items = c(
    "14, 16, 18, 20",
    " 3, 10, 19, 25",
    " 6,  8, 26, 28",
    "11, 24, 29, 30",
    " 5, 17, 27, 31",
    " 7, 13, 21, 22",
    " 2,  9, 15, 23",
    " 1,  4, 12, 32"
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

iis32_norms <- tibble::tibble(
  Sample = rep(1, 8),
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  M = c(4.25, 4.02, 4.26, 4.29, 4.66, 4.65, 4.42, 4.09),
  SD = c(0.99, 0.94, 0.82, 0.88, 0.86, 0.96, 0.87, 0.99)
)

iis32_norms_src <- tibble::tibble(
  Sample = 1,
  Size = 1380,
  Population = "American college students",
  Reference = "Hatcher & Rogers (2012)",
  URL = "https://doi.org/10.1080/00223891.2012.681818"
)

iis32_anchors <- tibble::tibble(
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

iis32_details <- tibble::tibble(
  Name = "Inventory of Interpersonal Strengths, Brief Version",
  Abbrev = "IIS-32",
  Items = 32,
  Scales = 8,
  Prefix = "",
  Suffix = "",
  Status = "partial text",
  Construct = "interpersonal strengths",
  Reference = "Hatcher & Rogers (2012)",
  URL = "https://doi.org/10.1080/00223891.2012.681818"
)

iis32_items <- tibble::tibble(
  Number = 1:32,
  Text = c(
    "I can really shine in the spotlight",
    "I enjoy being with other people",
    "I can make a decision even if others disagree",
    "I feel comfortable being open about myself",
    "I am able to compromise",
    "I can help others with their needs without neglecting my own",
    "I enjoy celebrating others' achievements",
    "I can say 'no' to others",
    "I'm warm with other people",
    "I argue effectively with others",
    "I can listen and think before I act in relationships",
    "I put myself out there in order to connect with others",
    "Offering other people emotional support is important to me",
    "I can be very persuasive",
    "I make time to be with others",
    "I am a strong but fair leader",
    "I show my gratitude for what others do for me",
    "I can ask other people for what I want",
    "I can take care of myself, even when others' needs feel pressing",
    "I can take charge in a group",
    "When friends ask for favors, I'm delighted to help them out",
    "I feel enriched by helping others",
    "I'm excited about meeting new people",
    "I recognize when others need privacy",
    "I can let other people know when I think that they're asking for too much from me",
    "I can rely on myself when I'm having problems with others",
    "I'm cooperative",
    "I'm ok with not being included in all activities",
    "I can be interested in others without being nosy",
    "I can resist others' tempting me to indulge myself",
    "I don't ask others for more than they are comfortable giving",
    "I put other people at ease"
  )
)

iis32 <- new_instrument(
  Scales = iis32_scales,
  Anchors = iis32_anchors,
  Items = iis32_items,
  Norms = list(iis32_norms, iis32_norms_src),
  Details = iis32_details
)

usethis::use_data(iis32, overwrite = TRUE)
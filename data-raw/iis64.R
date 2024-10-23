iis64_scales <- data.frame(
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

iis64_norms <- data.frame(
  Sample = rep(1, 8),
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  M = c(4.20, 4.10, 4.10, 4.23, 4.59, 4.66, 4.61, 4.16),
  SD = c(1.32, 1.29, 1.34, 1.24, 1.17, 1.14, 1.24, 1.36)
)

iis64_norms_src <- data.frame(
  Sample = 1,
  Size = 684,
  Population = "American college students",
  Reference = "Hatcher & Rogers (2009)",
  URL = "https://doi.org/10.1037/a0017269"
)

iis64_anchors <- data.frame(
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

iis64_details <- data.frame(
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

iis64_items <- data.frame(
  Number = 1:64,
  Text = c(
    "I can really shine in the spotlight",
    "I enjoy learning from people who have more experience than I do",
    "It's important to me to be honest even when it's difficult",
    "I recover quickly when people hurt my feelings",
    "I realize ",
    "I enjoy being with other people",
    "I can assert my needs even when it's not agreeable with others",
    "When someone irritates me, I look for a constructive solution",
    "I'm pretty even-tempered with others",
    "I can make a decision even if others disagree",
    "I don't give up easily in competitive situations",
    "I feel comfortable being open about myself",
    "I feel good when I'm with other people",
    "I am able to compromise",
    "I can help others with their needs without neglecting my own",
    "I feel confident in front of other people",
    "I enjoy celebrating others' achievements",
    "I can say 'no' to others",
    "I'm warm with other people",
    "I argue effectively with others",
    "I can listen and think before I act in relationships",
    "I enjoy lively competition with others",
    "I look forward to spending time with people",
    "I put myself out there in order to connect with others",
    "I try to regain contact with people with whom I've lost touch",
    "Offering other people emotional support is important to me",
    "I stick by my friends when they're in trouble",
    "I know how to look after my own interests",
    "I can be very persuasive",
    "I make time to be with others",
    "I am a strong but fair leader",
    "I show my gratitude for what others do for me",
    "It makes me happy when others are happy",
    "I hesitate to express opinions about others without all the facts",
    "My feelings of gratitude warm my relationships with others",
    "I like to be clear on my agreements with other people",
    "I can ask other people for what I want",
    "I enjoy mingling at parties",
    "I can take care of myself, even when others' needs feel pressing",
    "I can take charge in a group",
    "I work really well as an assistant",
    "When friends ask for favors, I'm delighted to help them out",
    "I feel enriched by helping others",
    "I'm excited about meeting new people",
    "I recognize when others need privacy",
    "I can let other people know when I think that they're asking for too much from me",
    "I'm a good listener",
    "I can rely on myself when I'm having problems with others",
    "I'm cooperative",
    "I'm ok with not being included in all activities",
    "I can be interested in others without being nosy",
    "I enjoy complimenting others",
    "I know how to be angry without pushing people away",
    "I can resist others' tempting me to indulge myself",
    "I'm comfortable disagreeing with others",
    "I'm respectful of others' need for time to themselves",
    "I approach other people with friendliness",
    "I am able to be assertive with other people",
    "When others get me down, I can bounce back",
    "I don't ask others for more than they are comfortable giving",
    "I like asking people about their lives",
    "I try to help people to loosen up",
    "I can make people laugh",
    "I put other people at ease"
  )
)

iis64 <- new_instrument(
  Scales = iis64_scales,
  Anchors = iis64_anchors,
  Items = iis64_items,
  Norms = list(iis64_norms, iis64_norms_src),
  Details = iis64_details
)

usethis::use_data(iis64, overwrite = TRUE)

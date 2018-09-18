csip_scales <- tibble(
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  Items = c(
    "1, 9, 17, 25, 33, 41, 49, 57",
    "2, 10, 18, 26, 34, 42, 50, 58",
    "3, 11, 19, 27, 35, 43, 51, 59",
    "4, 12, 20, 28, 36, 44, 52, 60",
    "5, 13, 21, 29, 37, 45, 53, 61",
    "6, 14, 22, 30, 38, 46, 54, 62",
    "7, 15, 23, 31, 39, 47, 55, 63",
    "8, 16, 24, 32, 40, 48, 56, 64"
  ),
  Label = c(
    "Domineering",
    "Self-Centered",
    "Distant",
    "Socially Inhibited",
    "Nonassertive",
    "Exploitable",
    "Self-Sacrificing",
    "Intrusive"
  )
)

csip_norms <- tibble(
  Sample = rep(1, 8),
  Scale = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  M = c(3.0, 3.2, 5.6, 7.2, 7.1, 6.5, 7.4, 4.7) / 8,
  SD = c(3.9, 3.8, 5.1, 5.5, 5.1, 4.6, 4.7, 4.0) / 8
)

csip_norms_src <- tibble(
  Sample = 1,
  Size = 712,
  Population = "American college students",
  Reference = "Boudreaux, Ozer, Oltmanns, & Wright (2018)",
  URL = "https://doi.org/10.1037/pas0000505"
)

csip_anchors <- tibble(
  Value = 0:3,
  Label = c(
    "Not a problem",
    "Minor problem",
    "Moderate problem",
    "Serious problem"
  )
)

csip_details <- list(
  Name = "Circumplex Scales of Interpersonal Problems",
  Abbrev = "CSIP",
  Items = 64,
  Scales = 8,
  Prefix = "",
  Suffix = "",
  Construct = "interpersonal problems",
  Reference = "Boudreaux, Ozer, Oltmanns, & Wright (2018)",
  URL = "https://doi.org/10.1037/pas0000505"
)

csip_items <- tibble(
  Number = 1:64,
  Text = c(
    "Bossing around other people too much",
    "Acting rude and inconsiderate toward others",
    "Pushing away from other people who get too close",
    "Difficulty making friends",
    "Lacking self-confidence",
    "Letting other people boss me around too much",
    "Putting other people's needs before my own too much",
    "Being overly affectionate with others",
    "Verbally or physically abusing others",
    "Acting selfishly with others",
    "Difficulty showing love and affection to others",
    "Having trouble fitting in with others",
    "Getting easily embarrassed in front of others",
    "Acting overly submissive with others",
    "Giving too much to others",
    "Difficulty keeping personal matters private from others",
    "Starting arguments and conflicts with others",
    "Being unable to feel guilt or remorse",
    "Being unable to enjoy the company of others",
    "Avoiding people or social situations",
    "Difficulty taking the lead",
    "Being unable to express anger toward others",
    "Forgiving people too easily",
    "Talking too much",
    "Trying to influence or control other people too much",
    "Lacking respect for other people's beliefs, attitudes, or opinions",
    "Feeling emotionally disconnected from others",
    "Being unable to keep conversations going",
    "Having trouble asserting myself",
    "Being too concerned about what other people think",
    "Being overly sentimental or tender-hearted",
    "Flirting with other people too much",
    "Dominating or intimidating others",
    "Having trouble getting along with others",
    "Difficulty developing close and lasting relationships",
    "Feeling like an outsider in most social situations",
    "Feeling weak and insecure around dominant others",
    "Being easily taken advantage of",
    "Being easily affected by the pain and suffering of others",
    "Having trouble respecting other people's privacy",
    "Acting aggressively toward others",
    "Being insensitive to the thoughts, feelings, and needs of others",
    "Being unable to fully connect with others",
    "Being unable to be myself around others",
    "Being unable to stand up to others",
    "Compromising with other people too much",
    "Trusting people too easily",
    "Exaggerating so that other people will respect me",
    "Manipulating other people to get what I want",
    "Disliking most people",
    "Difficulty opening up to others",
    "Feeling fearful or nervous in social situations",
    "Avoiding confrontation when problems arise",
    "Being easily influenced by others",
    "Trying to solve other people's problems too much",
    "Confronting people too quickly about problems",
    "Acting superior or condescending toward others",
    "Having trouble giving emotional or moral support to others",
    "Feeling uncomfortable with being close or intimate with others",
    "Acting shy around others",
    "Letting other people make decisions too often",
    "Being unable to say 'no'",
    "Getting too attached to others",
    "Needing to be the center of attention"
  )
)

csip <- new_instrument(
  Scales = csip_scales,
  Anchors = csip_anchors,
  Items = csip_items,
  Norms = list(csip_norms, csip_norms_src),
  Details = csip_details
)
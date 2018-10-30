csip_scales <- tibble::tibble(
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

csip_norms <- tibble::tibble(
  Sample = rep(1, 8),
  Scale = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  M = c(3.0, 3.2, 5.6, 7.2, 7.1, 6.5, 7.4, 4.7) / 8,
  SD = c(3.9, 3.8, 5.1, 5.5, 5.1, 4.6, 4.7, 4.0) / 8
)

csip_norms_src <- tibble::tibble(
  Sample = 1,
  Size = 712,
  Population = "American college students",
  Reference = "Boudreaux, Ozer, Oltmanns, & Wright (2018)",
  URL = "https://doi.org/10.1037/pas0000505"
)

csip_anchors <- tibble::tibble(
  Value = 0:3,
  Label = c(
    "Not a problem",
    "Minor problem",
    "Moderate problem",
    "Serious problem"
  )
)

csip_details <- tibble::tibble(
  Name = "Circumplex Scales of Interpersonal Problems",
  Abbrev = "CSIP",
  Items = 64,
  Scales = 8,
  Prefix = "",
  Suffix = "",
  Status = "open-access",
  Construct = "interpersonal problems",
  Reference = "Boudreaux, Ozer, Oltmanns, & Wright (2018)",
  URL = "https://doi.org/10.1037/pas0000505"
)

csip_items <- tibble::tribble(
  ~Number, ~Text,
  1, "Bossing around other people too much",
  2, "Acting rude and inconsiderate toward others",
  3, "Pushing away from other people who get too close",
  4, "Difficulty making friends",
  5, "Lacking self-confidence",
  6, "Letting other people boss me around too much",
  7, "Putting other people's needs before my own too much",
  8, "Being overly affectionate with others",
  9, "Verbally or physically abusing others",
  10, "Acting selfishly with others",
  11, "Difficulty showing love and affection to others",
  12, "Having trouble fitting in with others",
  13, "Getting easily embarrassed in front of others",
  14, "Acting overly submissive with others",
  15, "Giving too much to others",
  16, "Difficulty keeping personal matters private from others",
  17, "Starting arguments and conflicts with others",
  18, "Being unable to feel guilt or remorse",
  19, "Being unable to enjoy the company of others",
  20, "Avoiding people or social situations",
  21, "Difficulty taking the lead",
  22, "Being unable to express anger toward others",
  23, "Forgiving people too easily",
  24, "Talking too much",
  25, "Trying to influence or control other people too much",
  26, "Lacking respect for other people's beliefs, attitudes, or opinions",
  27, "Feeling emotionally disconnected from others",
  28, "Being unable to keep conversations going",
  29, "Having trouble asserting myself",
  30, "Being too concerned about what other people think",
  31, "Being overly sentimental or tender-hearted",
  32, "Flirting with other people too much",
  33, "Dominating or intimidating others",
  34, "Having trouble getting along with others",
  35, "Difficulty developing close and lasting relationships",
  36, "Feeling like an outsider in most social situations",
  37, "Feeling weak and insecure around dominant others",
  38, "Being easily taken advantage of",
  39, "Being easily affected by the pain and suffering of others",
  40, "Having trouble respecting other people's privacy",
  41, "Acting aggressively toward others",
  42, "Being insensitive to the thoughts, feelings, and needs of others",
  43, "Being unable to fully connect with others",
  44, "Being unable to be myself around others",
  45, "Being unable to stand up to others",
  46, "Compromising with other people too much",
  47, "Trusting people too easily",
  48, "Exaggerating so that other people will respect me",
  49, "Manipulating other people to get what I want",
  50, "Disliking most people",
  51, "Difficulty opening up to others",
  52, "Feeling fearful or nervous in social situations",
  53, "Avoiding confrontation when problems arise",
  54, "Being easily influenced by others",
  55, "Trying to solve other people's problems too much",
  56, "Confronting people too quickly about problems",
  57, "Acting superior or condescending toward others",
  58, "Having trouble giving emotional or moral support to others",
  59, "Feeling uncomfortable with being close or intimate with others",
  60, "Acting shy around others",
  61, "Letting other people make decisions too often",
  62, "Being unable to say 'no'",
  63, "Getting too attached to others",
  64, "Needing to be the center of attention"
)

csip <- new_instrument(
  Scales = csip_scales,
  Anchors = csip_anchors,
  Items = csip_items,
  Norms = list(csip_norms, csip_norms_src),
  Details = csip_details
)

usethis::use_data(csip, overwrite = TRUE)
# IIP-32 (M75).
#
# Source: Horowitz, Alden, Wiggins & Pincus (2003), *Inventory of Interpersonal
# Problems manual* (3rd ed., Mind Garden); see cairn/references/horowitz2003.md
# for every value's page anchor. Item map: Appendix H, pp. 101-102.
#
# Divisor 4: Table F.5 (p. 91) prints raw scale SUMS over each scale's four
# items on a 0-4 anchor range, and the package ships item means, so every mean
# and SD below is the printed value divided by 4. Table F.5 sits at the END of
# Appendix F, after that appendix's T-score conversion tables, and is omitted
# from the manual's own contents listing -- which is why the IIP-32 was long
# thought to have no published descriptives.
#
# The normative means and standard deviations are reproduced under the credit
# line the publisher's permission requires:
#
#   "Reproduction by special permission of the Publisher, Mind Garden, Inc.,
#   www.mindgarden.com from the Inventory of Interpersonal Problems by Leonard
#   M. Horowitz, Lynn E. Alden, Jerry S. Wiggins, & Aaron L. Pincus. Copyright
#   © 2000 by Leonard M. Horowitz, Lynn E. Alden, Jerry S. Wiggins, & Aaron L.
#   Pincus. Further Reproduction is prohibited without the Publisher's written
#   consent."
#
# The permission covers the means and SDs only. Item TEXT is not licensed and is
# not shipped; iip32_items is a pointer row. The item NUMBERS in Scales$Items
# are the scoring key, which is permitted.

iip32_scales <- data.frame(
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  Items = c(
    "22, 25, 28, 30",
    "14, 16, 17, 18",
    "10, 11, 13, 15",
    "2, 5, 9, 19",
    "4, 6, 7, 12",
    "1, 8, 20, 31",
    "23, 26, 27, 32",
    "3, 21, 24, 29"
  ),
  Label = c(
    "Domineering/Controlling",
    "Vindictive/Self-Centered",
    "Cold/Distant",
    "Socially Inhibited",
    "Nonassertive",
    "Overly Accommodating",
    "Self-Sacrificing",
    "Intrusive/Needy"
  )
)

iip32_norms <- data.frame(
  Sample = c(rep(1, 8), rep(2, 8), rep(3, 8)),
  Scale = rep(c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"), 3),
  Angle = rep(c(90, 135, 180, 225, 270, 315, 360, 45), 3),
  M = c(
    2.0, 2.7, 2.7, 3.3, 4.0, 4.3, 4.3, 2.7,
    1.8, 2.0, 2.7, 3.0, 4.3, 4.8, 4.7, 2.5,
    2.3, 3.0, 3.0, 3.3, 3.7, 4.0, 3.8, 2.8
  ) / 4,
  SD = c(
    2.5, 3.3, 3.7, 3.3, 3.3, 3.0, 3.3, 2.6,
    2.5, 3.3, 3.3, 3.3, 3.7, 3.3, 3.3, 2.8,
    2.5, 3.3, 3.7, 3.7, 3.3, 3.0, 3.3, 2.8
  ) / 4
)

iip32_norms_src <- data.frame(
  Sample = c(1, 2, 3),
  Size = c(800, 400, 400),
  Population = c(
    "American adults, national standardization sample, overall",
    "American adults, national standardization sample, females",
    "American adults, national standardization sample, males"
  ),
  Reference = "Horowitz, Alden, Wiggins, & Pincus (2003)",
  URL = "https://www.mindgarden.com/113-inventory-of-interpersonal-problems"
)

iip32_anchors <- data.frame(
  Value = 0:4,
  Label = c(
    "Not at all",
    "A little bit",
    "Moderately",
    "Quite a bit",
    "Extremely"
  )
)

iip32_details <- data.frame(
  Name = "Inventory of Interpersonal Problems, Brief Version",
  Abbrev = "IIP-32",
  Items = 32,
  Scales = 8,
  Prefix = "",
  Suffix = "",
  Status = "copyrighted",
  Construct = "interpersonal problems",
  Reference = "Horowitz, Alden, Wiggins, & Pincus (2003)",
  URL = "https://www.mindgarden.com/113-inventory-of-interpersonal-problems"
)

iip32_items <- data.frame(
  Number = NA,
  Text = "Visit the Mind Garden Inc. website for item text and numbering."
)

iip32 <- new_instrument(
  Scales = iip32_scales,
  Anchors = iip32_anchors,
  Items = iip32_items,
  Norms = list(iip32_norms, iip32_norms_src),
  Details = iip32_details
)

usethis::use_data(iip32, overwrite = TRUE)

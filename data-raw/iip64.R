# IIP-64 (M75).
#
# Source: Horowitz, Alden, Wiggins & Pincus (2003), *Inventory of Interpersonal
# Problems manual* (3rd ed., Mind Garden); see cairn/references/horowitz2003.md
# for every value's page anchor. Item map: Appendix A, pp. 57-59.
#
# Divisor 8: Table 4.4 (pp. 27-29) prints raw scale SUMS over each scale's eight
# items on a 0-4 anchor range, and the package ships item means, so every mean
# and SD below is the printed value divided by 8. Samples 1-3 are the manual's
# Overall (N = 800), Female (n = 400) and Male (n = 400) norm groups -- three
# groupings of one national standardization sample, not three recruitments.
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
# not shipped; iip64_items is a pointer row. The item NUMBERS in Scales$Items
# are the scoring key, which is permitted.

iip64_scales <- data.frame(
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  Items = c(
    "17, 31, 44, 45, 50, 52, 57, 59",
    "1, 22, 24, 29, 32, 40, 56, 64",
    "11, 15, 16, 20, 23, 27, 36, 60",
    "3, 7, 14, 18, 33, 35, 55, 62",
    "5, 6, 8, 9, 12, 13, 19, 39",
    "2, 10, 25, 34, 38, 42, 53, 61",
    "21, 28, 37, 46, 49, 51, 54, 63",
    "4, 26, 30, 41, 43, 47, 48, 58"
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

iip64_norms <- data.frame(
  Sample = c(rep(1, 8), rep(2, 8), rep(3, 8)),
  Scale = rep(c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"), 3),
  Angle = rep(c(90, 135, 180, 225, 270, 315, 360, 45), 3),
  M = c(
    4.9, 5.3, 5.7, 6.5, 7.4, 7.8, 8.2, 5.7,
    4.5, 4.8, 5.1, 6.4, 8.0, 8.6, 8.8, 5.4,
    5.3, 5.8, 6.3, 6.6, 6.8, 7.1, 7.7, 5.9
  ) / 8,
  SD = c(
    4.5, 5.1, 5.9, 5.7, 6.1, 5.3, 5.5, 4.8,
    4.1, 4.9, 5.6, 5.7, 6.1, 5.4, 5.5, 4.6,
    4.7, 5.2, 6.1, 5.7, 6.1, 5.1, 5.4, 5.0
  ) / 8
)

iip64_norms_src <- data.frame(
  Sample = c(1, 2, 3),
  Size = c(800, 400, 400),
  Population = c(
    "American adults, national standardization sample, overall",
    "American adults, national standardization sample, females",
    "American adults, national standardization sample, males"
  ),
  Reference = "Horowitz, Alden, Wiggins, & Pincus (2003)",
  URL = "https://www.mindgarden.com/113-inventory-of-interpersonal-problems",
  # Kind: `standardization` all three -- the national standardization sample of
  # 800 cases described on p. 25, whose octant statistics are Table 4.4
  # (pp. 27-29). With the IIP-32, the only shipped samples drawn to represent a
  # defined population.
  # The assignment and its basis are recorded in
  # cairn/references/norms-audit.md's Reference kind table;
  # data-raw/derive-norms-kind.R diffs this column against that record.
  Kind = c("standardization", "standardization", "standardization")
)

iip64_anchors <- data.frame(
  Value = 0:4,
  Label = c(
    "Not at all",
    "A little bit",
    "Moderately",
    "Quite a bit",
    "Extremely"
  )
)

iip64_details <- data.frame(
  Name = "Inventory of Interpersonal Problems",
  Abbrev = "IIP-64",
  Items = 64,
  Scales = 8,
  Prefix = "",
  Suffix = "",
  Status = "copyrighted",
  Construct = "interpersonal problems",
  Reference = "Horowitz, Alden, Wiggins, & Pincus (2003)",
  URL = "https://www.mindgarden.com/113-inventory-of-interpersonal-problems"
)

iip64_items <- data.frame(
  Number = NA,
  Text = "Visit the Mind Garden Inc. website for item text and numbering."
)

iip64 <- new_instrument(
  Scales = iip64_scales,
  Anchors = iip64_anchors,
  Items = iip64_items,
  Norms = list(iip64_norms, iip64_norms_src),
  Details = iip64_details
)

usethis::use_data(iip64, overwrite = TRUE)

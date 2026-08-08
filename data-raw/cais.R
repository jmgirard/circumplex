cais_scales <- data.frame(
  Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  Angle = c(90, 135, 180, 225, 270, 315, 360, 45),
  # NOT the eight-cycle "1, 9, 17, 25" template the package's other 32-item
  # instruments use. The CAIS has 37 items in octant blocks of UNEQUAL size --
  # five each for PA, BC, DE, HI, LM and NO, four for FG and three for JK
  # (sodano2006 Table 1, p. 322) -- and the item ordering below is a round-robin
  # over those blocks that correctly skips an octant once its items run out.
  # The eight-cycle key reproduced that ordering only through item 29, then ran
  # one position out: it put item 30 (an LM item) in JK, 31 (NO) in LM and 32
  # (PA) in NO, and left items 33-37 in no scale at all (M74).
  Items = c(
    "1,  9, 17, 25, 32",
    "2, 10, 18, 26, 33",
    "3, 11, 19, 27, 34",
    "4, 12, 20, 28",
    "5, 13, 21, 29, 35",
    "6, 14, 22",
    "7, 15, 23, 30, 36",
    "8, 16, 24, 31, 37"
  ),
  Label = c(
    "Assured-Dominant",
    "Arrogant-Calculating",
    "Cold-Hearted",
    "Aloof-Introverted",
    "Unassured-Submissive",
    "Unassuming-Ingenuous",
    "Warm-Agreeable",
    "Gregarious-Extraverted"
  )
)

# Item means, not sums: these are sodano2006's Table 2 and Table 4 values
# verbatim, and the CAIS anchors run 1-5, which the octant means below sit
# inside (M74; the audit therefore carries no divisor for either sample).
cais_norms <- data.frame(
  Sample = c(rep(1, 8), rep(2, 8)),
  Scale = rep(c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"), 2),
  Angle = rep(c(90, 135, 180, 225, 270, 315, 360, 45), 2),
  M = c(
    c(3.39, 2.11, 1.85, 1.99, 2.08, 2.76, 3.62, 3.75),
    c(5.19, 3.97, 2.34, 2.76, 3.87, 4.16, 6.52, 6.14)
  ),
  SD = c(
    c(0.84, 0.85, 0.77, 0.74, 0.64, 0.81, 0.86, 0.73),
    c(0.89, 1.08, 0.98, 1.11, 1.12, 0.99, 0.93, 0.87)
  )
)

cais_norms_src <- data.frame(
  Sample = c(1, 2),
  # 204, not 213: sodano2006 gives the child sample as 213 on pp. 320-321, but
  # the note to Table 2 -- the table the sample-1 M and SD above are read from
  # -- says N = 204, and the article never reconciles them. The size that
  # describes the sample these statistics were computed on is the table's own
  # (M74). Sample 2 has no such split: p. 320, p. 321 and Table 4 all say 194.
  Size = c(204, 194),
  Population = c(
    "American fourth and sixth graders (aged 9 to 13)",
    "American college students (aged 17 to 50)"
  ),
  Reference = c(
    "Sodano & Tracey (2006)",
    "Sodano & Tracey (2006)"
  ),
  URL = c(
    "https://doi.org/10.1207/s15327752jpa8703_12",
    "https://doi.org/10.1207/s15327752jpa8703_12"
  ),
  # Kind: both samples' octant statistics are printed in sodano2006 (Table 2,
  # children; Table 4, adults), so both are `published`. Sample 2 is separately
  # refused at standardization for leaving the response range, which is a metric
  # problem and does not change what kind of reference the sample is.
  # The assignment and its basis are recorded in
  # cairn/references/norms-audit.md's Reference kind table;
  # data-raw/derive-norms-kind.R diffs this column against that record.
  Kind = c("published", "published")
)

cais_anchors <- data.frame(
  Value = 1:5,
  Label = c("Never", "A little", "Some", "A lot", "Always")
)

cais_details <- data.frame(
  Name = "Child and Adolescent Interpersonal Survey",
  Abbrev = "CAIS",
  Items = 37,
  Scales = 8,
  Prefix = "",
  Suffix = "",
  Status = "open-access",
  Construct = "interpersonal traits",
  Reference = "Sodano & Tracey (2006)",
  URL = "https://doi.org/10.1207/s15327752jpa8703_12"
)

# TODO: Check permission to release item text
cais_items <- data.frame(
  Number = 1:37,
  Text = c(
    "I am tough",
    "I call people names",
    "I hurt people",
    "I am by myself a lot",
    "I am shy",
    "I am calm",
    "I am kind to others",
    "I am fun to be around",
    "I know a lot",
    "I like making trouble",
    "I make people cry",
    "I am alone",
    "I am sad",
    "I am quiet",
    "I try to help others feel better",
    "I am happy",
    "I think I can do a lot",
    "I trick people",
    "I am mean to others",
    "I am hard to get to know",
    "I know very little",
    "Tricking people is mean",
    "I am friendly",
    "I am giving",
    "I speak up for myself",
    "I tell people what to do",
    "I like it when others feel bad",
    "I play by myself",
    "I give in easily",
    "I help people",
    "I play with others",
    "I think I am right",
    "I am sneaky",
    "I am grumpy",
    "I am afraid",
    "I share",
    "I have a lot of friends"
  )
)

cais <- new_instrument(
  Scales = cais_scales,
  Anchors = cais_anchors,
  Items = cais_items,
  Norms = list(cais_norms, cais_norms_src),
  Details = cais_details
)

usethis::use_data(cais, overwrite = TRUE)

# Norms provenance pins (M72).
#
# These lock the FULL Norms and Scales objects of every instrument whose norms
# have been verified against a published source, so a later silent edit to any
# shipped value -- a mean, an angle, an item map, the sample size, the
# population label, the provenance reference or URL, or the Sample key that
# joins the two norms frames -- fails here rather than reaching users.
#
# Traceability: every value below is a row of data-raw/norms-audit-ledger.csv,
# which carries the source value and its page/table anchor for each audited
# field, or a value the ledger records as not-published-in-source or as an
# approved intended-deviation. The source anchors themselves live in
# cairn/references/<citekey>.md; the comparison that established them is
# data-raw/audit-norms.R. Pinning the whole object rather than a field list is
# deliberate: a field list pins what its author remembered to name.

# The shipped instruments are enumerated by the same procedure instruments()
# uses -- data() plus a class filter -- rather than a hand-list, so a newly
# added instrument is caught by the completeness test below instead of
# silently going unpinned. (instruments() itself prints and returns NULL, so
# its return value cannot be used here.)
shipped_instruments <- function() {
  nms <- utils::data(package = "circumplex")$results[, "Item"]
  sort(Filter(function(nm) {
    e <- new.env()
    utils::data(list = nm, package = "circumplex", envir = e)
    inherits(get(nm, envir = e), "circumplex_instrument")
  }, nms))
}

audited_objects <- list(
  csie = list(
    Norms = 
      list(structure(list(Sample = c(1, 1, 1, 1, 1, 1, 1, 1), Abbrev = c("PA", 
      "BC", "DE", "FG", "HI", "JK", "LM", "NO"), Angle = c(90, 135, 
      180, 225, 270, 315, 360, 45), M = c(7.23, 6.44, 6.93, 7.24, 7.31, 
      8.51, 7.9, 7.3), SD = c(1.68, 1.66, 1.82, 1.54, 1.53, 1.11, 1.2, 
      1.37)), class = "data.frame", row.names = c(NA, -8L)), structure(list(
          Sample = 1, Size = 367, Population = "American college students", 
          Reference = "Locke & Sadler (2007)", URL = "https://kennethlocke.org/CSIE/CSIE_Norms.html"), class = "data.frame", row.names = c(NA, 
      -1L)))
    ,
    Scales = 
      structure(list(Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", 
      "LM", "NO"), Angle = c(90, 135, 180, 225, 270, 315, 360, 45), 
          Items = c("4, 12, 20, 28", "7, 15, 23, 31", "2, 10, 18, 26", 
          "5, 13, 21, 29", "8, 16, 24, 32", "3, 11, 19, 27", "6, 14, 22, 30", 
          "1,  9, 17, 25"), Label = c("+A", "+A-C", "-C", "-A-C", "-A", 
          "-A+C", "+C", "+A+C")), class = "data.frame", row.names = c(NA, 
      -8L))
  ),
  csig = list(
    Norms = 
      list(structure(list(Sample = c(1, 1, 1, 1, 1, 1, 1, 1), Abbrev = c("PA", 
      "BC", "DE", "FG", "HI", "JK", "LM", "NO"), Angle = c(90, 135, 
      180, 225, 270, 315, 360, 45), M = c(2.96, 2.53, 2.02, 1.88, 2.24, 
      2.89, 2.97, 2.96), SD = c(0.68, 0.86, 0.88, 0.74, 0.9, 0.76, 
      0.71, 0.68)), class = "data.frame", row.names = c(NA, -8L)), 
          structure(list(Sample = 1, Size = 665, Population = "MTurkers from US, Canada, and India about interactions between nations", 
              Reference = "Locke (2014)", URL = "https://doi.org/10.1177/0146167213514280"), class = "data.frame", row.names = c(NA, 
          -1L)))
    ,
    Scales = 
      structure(list(Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", 
      "LM", "NO"), Angle = c(90, 135, 180, 225, 270, 315, 360, 45), 
          Items = c("8, 16, 24, 32", "5, 13, 21, 29", "2, 10, 18, 26", 
          "7, 15, 23, 31", "4, 12, 20, 28", "1, 9, 17, 25", "6, 14, 22, 30", 
          "3, 11, 19, 27"), Label = c("Be authoritative", "Be tough", 
          "Be self-protective", "Be wary", "Be conflict-avoidant", 
          "Be cooperative", "Be understanding", "Be respected")), class = "data.frame", row.names = c(NA, 
      -8L))
  ),
  csip = list(
    Norms = 
      list(structure(list(Sample = c(1, 1, 1, 1, 1, 1, 1, 1), Scale = c("PA", 
      "BC", "DE", "FG", "HI", "JK", "LM", "NO"), Angle = c(90, 135, 
      180, 225, 270, 315, 360, 45), M = c(0.375, 0.4, 0.7, 0.9, 0.8875, 
      0.8125, 0.925, 0.5875), SD = c(0.4875, 0.475, 0.6375, 0.6875, 
      0.6375, 0.575, 0.5875, 0.5)), class = "data.frame", row.names = c(NA, 
      -8L)), structure(list(Sample = 1, Size = 712, Population = "American college students", 
          Reference = "Boudreaux, Ozer, Oltmanns, & Wright (2018)", 
          URL = "https://doi.org/10.1037/pas0000505"), class = "data.frame", row.names = c(NA, 
      -1L)))
    ,
    Scales = 
      structure(list(Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", 
      "LM", "NO"), Angle = c(90, 135, 180, 225, 270, 315, 360, 45), 
          Items = c("1, 9, 17, 25, 33, 41, 49, 57", "2, 10, 18, 26, 34, 42, 50, 58", 
          "3, 11, 19, 27, 35, 43, 51, 59", "4, 12, 20, 28, 36, 44, 52, 60", 
          "5, 13, 21, 29, 37, 45, 53, 61", "6, 14, 22, 30, 38, 46, 54, 62", 
          "7, 15, 23, 31, 39, 47, 55, 63", "8, 16, 24, 32, 40, 48, 56, 64"
          ), Label = c("Domineering", "Self-Centered", "Distant", "Socially Inhibited", 
          "Nonassertive", "Exploitable", "Self-Sacrificing", "Intrusive"
          )), class = "data.frame", row.names = c(NA, -8L))
  ),
  csiv = list(
    Norms = 
      list(structure(list(Sample = c(1, 1, 1, 1, 1, 1, 1, 1), Abbrev = c("PA", 
      "BC", "DE", "FG", "HI", "JK", "LM", "NO"), Angle = c(90, 135, 
      180, 225, 270, 315, 360, 45), M = c(2.53, 1.38, 1.1, 1.66, 1.77, 
      2.67, 2.83, 2.93), SD = c(0.63, 0.71, 0.7, 0.78, 0.75, 0.71, 
      0.69, 0.57)), class = "data.frame", row.names = c(NA, -8L)), 
          structure(list(Sample = 1, Size = 1200, Population = "American college students", 
              Reference = "Locke (n.d.); instrument published as Locke (2000)", 
              URL = "https://kennethlocke.org/CSIV/CSIV_Norms.html"), class = "data.frame", row.names = c(NA, 
          -1L)))
    ,
    Scales = 
      structure(list(Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", 
      "LM", "NO"), Angle = c(90, 135, 180, 225, 270, 315, 360, 45), 
          Items = c("1,  9, 17, 25, 33, 41, 49, 57", "4, 12, 20, 28, 36, 44, 52, 60", 
          "7, 15, 23, 31, 39, 47, 55, 63", "2, 10, 18, 26, 34, 42, 50, 58", 
          "5, 13, 21, 29, 37, 45, 53, 61", "8, 16, 24, 32, 40, 48, 56, 64", 
          "3, 11, 19, 27, 35, 43, 51, 59", "6, 14, 22, 30, 38, 46, 54, 62"
          ), Label = c("+A", "+A-C", "-C", "-A-C", "-A", "-A+C", "+C", 
          "+A+C")), class = "data.frame", row.names = c(NA, -8L))
  ),
  iitc = list(
    Norms = 
      list(structure(list(Sample = c(1, 1, 1, 1, 1, 1, 1, 1), Abbrev = c("PA", 
      "BC", "DE", "FG", "HI", "JK", "LM", "NO"), Angle = c(90, 135, 
      180, 225, 270, 315, 360, 45), M = c(1.18, 0.83, 0.76, 0.85, 1.24, 
      2.13, 2.66, 1.88), SD = c(0.81, 0.77, 0.73, 0.74, 0.78, 0.86, 
      0.85, 1.02)), class = "data.frame", row.names = c(NA, -8L)), 
          structure(list(Sample = 1, Size = 862, Population = "American college students", 
              Reference = "Bliton & Pincus (2019)", URL = "https://doi.org/10.1177/1073191119864661"), class = "data.frame", row.names = c(NA, 
          -1L)))
    ,
    Scales = 
      structure(list(Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", 
      "LM", "NO"), Angle = c(90, 135, 180, 225, 270, 315, 360, 45), 
          Items = c(" 1,  9, 17, 25, 33, 41, 49, 57", " 2, 10, 18, 26, 34, 42, 50, 58", 
          " 3, 11, 19, 27, 35, 43, 51, 59", " 4, 12, 20, 28, 36, 44, 52, 60", 
          " 5, 13, 21, 29, 37, 45, 53, 61", " 6, 14, 22, 30, 38, 46, 54, 62", 
          " 7, 15, 23, 31, 39, 47, 55, 63", " 8, 16, 24, 32, 40, 48, 56, 64"
          ), Label = c("Dominant", "Calculating", "Cold", "Self-Critical", 
          "Submissive", "Ingratiating", "Warm", "Gregarious")), class = "data.frame", row.names = c(NA, 
      -8L))
  ),
  iis32 = list(
    Norms = 
      list(structure(list(Sample = c(1, 1, 1, 1, 1, 1, 1, 1), Abbrev = c("PA", 
      "BC", "DE", "FG", "HI", "JK", "LM", "NO"), Angle = c(90, 135, 180, 225, 
      270, 315, 360, 45), M = c(4.25, 4.02, 4.26, 4.29, 4.66, 4.65, 4.42, 4.09
      ), SD = c(0.99, 0.94, 0.82, 0.88, 0.86, 0.96, 0.87, 0.99)), class = "data.frame", row.names = c(NA, 
      -8L)), structure(list(Sample = 1, Size = 1380, Population = "American college students", 
          Reference = "Norms source unconfirmed; instrument published as Hatcher & Rogers (2012)", 
          URL = "https://doi.org/10.1080/00223891.2012.681818"), class = "data.frame", row.names = c(NA, 
      -1L)))
    ,
    Scales = 
      structure(list(Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"
      ), Angle = c(90, 135, 180, 225, 270, 315, 360, 45), Items = c("14, 16, 18, 20", 
      " 3, 10, 19, 25", " 6,  8, 26, 28", "11, 24, 29, 30", " 5, 17, 27, 31", 
      " 7, 13, 21, 22", " 2,  9, 15, 23", " 1,  4, 12, 32"), Label = c("Lead", 
      "Direct", "Balance", "Restrain", "Cooperate", "Consider", "Connect", 
      "Engage")), class = "data.frame", row.names = c(NA, -8L))
  ),
  iis64 = list(
    Norms = 
      list(structure(list(Sample = c(1, 1, 1, 1, 1, 1, 1, 1), Abbrev = c("PA", 
      "BC", "DE", "FG", "HI", "JK", "LM", "NO"), Angle = c(90, 135, 180, 225, 
      270, 315, 360, 45), M = c(4.2, 4.1, 4.1, 4.23, 4.59, 4.66, 4.61, 4.16
      ), SD = c(1.32, 1.29, 1.34, 1.24, 1.17, 1.14, 1.24, 1.36)), class = "data.frame", row.names = c(NA, 
      -8L)), structure(list(Sample = 1, Size = 684, Population = "American college students", 
          Reference = "Hatcher & Rogers (2009)", URL = "https://doi.org/10.1037/a0017269"), class = "data.frame", row.names = c(NA, 
      -1L)))
    ,
    Scales = 
      structure(list(Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"
      ), Angle = c(90, 135, 180, 225, 270, 315, 360, 45), Items = c("11, 16, 22, 29, 31, 37, 40, 58", 
      " 7, 10, 20, 28, 39, 46, 55, 59", " 4,  5, 15, 18, 36, 48, 50, 53", " 3,  8, 21, 34, 45, 51, 54, 56", 
      " 2,  9, 14, 32, 41, 47, 49, 60", "17, 26, 27, 33, 35, 42, 43, 52", " 6, 13, 19, 23, 30, 44, 57, 61", 
      " 1, 12, 24, 25, 38, 62, 63, 64"), Label = c("Lead", "Direct", "Balance", 
      "Restrain", "Cooperate", "Consider", "Connect", "Engage")), class = "data.frame", row.names = c(NA, 
      -8L))
  ),
  ipipipc = list(
    Norms = 
      list(structure(list(Sample = c(1, 1, 1, 1, 1, 1, 1, 1), Scale = c("PA", 
      "BC", "DE", "FG", "HI", "JK", "LM", "NO"), Angle = c(90, 135, 180, 225, 
      270, 315, 360, 45), M = c(2.66, 2.27, 2.46, 2.68, 3.2, 3.64, 4.37, 3.64
      ), SD = c(0.71, 0.69, 0.58, 0.79, 0.63, 0.58, 0.47, 0.78)), class = "data.frame", row.names = c(NA, 
      -8L)), structure(list(Sample = 1, Size = 274, Population = "American college students", 
          Reference = "Norms source unconfirmed; instrument published as Markey & Markey (2009)", 
          URL = "https://doi.org/10.1177/1073191109340382"), class = "data.frame", row.names = c(NA, 
      -1L)))
    ,
    Scales = 
      structure(list(Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"
      ), Angle = c(90, 135, 180, 225, 270, 315, 360, 45), Items = c("6, 14, 22, 30", 
      "7, 15, 23, 31", "8, 16, 24, 32", "1, 9, 17, 25", "2, 10, 18, 26", "3, 11, 19, 27", 
      "4, 12, 20, 28", "5, 13, 21, 29"), Label = c("Assured-Dominant", "Arrogant-Calculating", 
      "Cold-Hearted", "Aloof-Introverted", "Unassured-Submissive", "Unassuming-Ingenuous", 
      "Warm-Agreeable", "Gregarious-Extraverted")), class = "data.frame", row.names = c(NA, 
      -8L))
  ),
  isc = list(
    Norms = 
      list(structure(list(Sample = c(1, 1, 1, 1, 1, 1, 1, 1), Scale = c("PA", 
      "BC", "DE", "FG", "HI", "JK", "LM", "NO"), Angle = c(90, 135, 180, 225, 
      270, 315, 360, 45), M = c(6.04, 6.17, 5.23, 4.44, 4.55, 3.91, 3.32, 5.31
      ), SD = c(1.06, 1.1, 1.28, 1.16, 1.24, 0.99, 0.91, 1)), class = "data.frame", row.names = c(NA, 
      -8L)), structure(list(Sample = 1, Size = 649, Population = "American college students", 
          Reference = "Hopwood et al. (2011)", URL = "https://doi.org/10.1111/j.1467-6494.2011.00696.x"), class = "data.frame", row.names = c(NA, 
      -1L)))
    ,
    Scales = 
      structure(list(Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"
      ), Angle = c(90, 135, 180, 225, 270, 315, 360, 45), Items = c("1,  9, 17, 25, 33, 41, 49, 57", 
      "2, 10, 18, 26, 34, 42, 50, 58", "3, 11, 19, 27, 35, 43, 51, 59", "4, 12, 20, 28, 36, 44, 52, 60", 
      "5, 13, 21, 29, 37, 45, 53, 61", "6, 14, 22, 30, 38, 46, 54, 62", "7, 15, 23, 31, 39, 47, 55, 63", 
      "8, 16, 24, 32, 40, 48, 56, 64"), Label = c("Sensitive to Control", "Sensitive to Antagonism", 
      "Sensitive to Remoteness", "Sensitive to Timidity", "Sensitive to Passivity", 
      "Sensitive to Dependence", "Sensitive to Affection", "Sensitive to Attention-Seeking"
      )), class = "data.frame", row.names = c(NA, -8L))
  ),
  cais = list(
    Norms = 
      list(structure(list(Sample = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 
      2, 2), Scale = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO", "PA", 
      "BC", "DE", "FG", "HI", "JK", "LM", "NO"), Angle = c(90, 135, 180, 225, 
      270, 315, 360, 45, 90, 135, 180, 225, 270, 315, 360, 45), M = c(3.39, 2.11, 
      1.85, 1.99, 2.08, 2.76, 3.62, 3.75, 5.19, 3.97, 2.34, 2.76, 3.87, 4.16, 
      6.52, 6.14), SD = c(0.84, 0.85, 0.77, 0.74, 0.64, 0.81, 0.86, 0.73, 0.89, 
      1.08, 0.98, 1.11, 1.12, 0.99, 0.93, 0.87)), class = "data.frame", row.names = c(NA, 
      -16L)), structure(list(Sample = c(1, 2), Size = c(204, 194), Population = c("American fourth and sixth graders (aged 9 to 13)", 
      "American college students (aged 17 to 50)"), Reference = c("Sodano & Tracey (2006)", 
      "Sodano & Tracey (2006)"), URL = c("https://doi.org/10.1207/s15327752jpa8703_12", 
      "https://doi.org/10.1207/s15327752jpa8703_12")), class = "data.frame", row.names = c(NA, 
      -2L)))
    ,
    Scales = 
      structure(list(Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"
      ), Angle = c(90, 135, 180, 225, 270, 315, 360, 45), Items = c("1,  9, 17, 25, 32", 
      "2, 10, 18, 26, 33", "3, 11, 19, 27, 34", "4, 12, 20, 28", "5, 13, 21, 29, 35", 
      "6, 14, 22", "7, 15, 23, 30, 36", "8, 16, 24, 31, 37"), Label = c("Assured-Dominant", 
      "Arrogant-Calculating", "Cold-Hearted", "Aloof-Introverted", "Unassured-Submissive", 
      "Unassuming-Ingenuous", "Warm-Agreeable", "Gregarious-Extraverted")), class = "data.frame", row.names = c(NA, 
      -8L))
  ),
  iei = list(
    Norms = 
      list(structure(list(Sample = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 
      2L, 2L, 2L, 2L, 2L, 2L), Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", 
      "LM", "NO", "PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"), Angle = c(90, 
      135, 180, 225, 270, 315, 360, 45, 90, 135, 180, 225, 270, 315, 360, 45), 
    M = c(2, 1.21, 0.91, 1.18, 2.03, 2.63, 2.7, 2.41, 1.82, 1.22, 1.08, 
    1.3, 1.83, 2.37, 2.43, 2.2), SD = c(0.71, 0.61, 0.68, 0.84, 0.86, 0.6, 
    0.66, 0.73, 0.79, 0.53, 0.66, 0.89, 0.9, 0.51, 0.68, 0.79)), class = "data.frame", row.names = c(NA, 
      -16L)), structure(list(Sample = c(1, 2), Size = c(1223, 278), Population = c("American undergraduate students", 
      "American crowdworkers"), Reference = c("Horner, Locke, & Hulsey (2024)", 
      "Horner, Locke, & Hulsey (2024)"), URL = c("https://kennethlocke.org/IEI/IEI_Norms.html", 
      "https://doi.org/10.1080/00223891.2024.2400266")), class = "data.frame", row.names = c(NA, 
      -2L)))
    ,
    Scales = 
      structure(list(Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"
      ), Angle = c(90, 135, 180, 225, 270, 315, 360, 45), Items = c("1, 2, 3, 4, 5, 6, 7, 8", 
      "9, 10, 11, 12, 13, 14, 15, 16", "17, 18, 19, 20, 21, 22, 23, 24", "25, 26, 27, 28, 29, 30, 31, 32", 
      "33, 34, 35, 36, 37, 38, 39, 40", "41, 42, 43, 44, 45, 46, 47, 48", "49, 50, 51, 52, 53, 54, 55, 56", 
      "57, 58, 59, 60, 61, 62, 63, 64"), Label = c("Confident-Impressive", "Superior-Callous", 
      "Rejecting-Suspicious", "Rejected-Ashamed", "Insecure-Anxious", "Needy-Empathic", 
      "Welcoming-Trusting", "Included-Proud")), class = "data.frame", row.names = c(NA, 
      -8L))
  ),
  igicr = list(
    Norms = 
      list(structure(list(Sample = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 
      2, 2, 3, 3, 3, 3, 3, 3, 3, 3), Scale = c("PA", "BC", "DE", "FG", "HI", 
      "JK", "LM", "NO", "PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO", "PA", 
      "BC", "DE", "FG", "HI", "JK", "LM", "NO"), Angle = c(90, 135, 180, 225, 
      270, 315, 360, 45, 90, 135, 180, 225, 270, 315, 360, 45, 90, 135, 180, 
      225, 270, 315, 360, 45), M = c(2.09, 0.97, 1.51, 2.23, 2.38, 2.74, 2.68, 
      2.35, 2.13, 1.11, 1.68, 2.24, 2.32, 2.64, 2.52, 2.29, 2.06, 0.85, 1.37, 
      2.23, 2.43, 2.82, 2.81, 2.4), SD = c(0.8, 0.66, 0.85, 0.91, 0.8, 0.76, 
      0.77, 0.76, 0.75, 0.68, 0.87, 0.92, 0.79, 0.7, 0.72, 0.73, 0.83, 0.61, 
      0.82, 0.91, 0.81, 0.8, 0.78, 0.78)), class = "data.frame", row.names = c(NA, 
      -24L)), structure(list(Sample = c(1, 2, 3), Size = c(387, 174, 213), Population = c("American community adolescents (age 11-13), overall", 
      "American community adolescents (age 11-13), males", "American community adolescents (age 11-13), females"
      ), Reference = c("Trucco, Wright, & Colder (2013)", "Trucco, Wright, & Colder (2013)", 
      "Trucco, Wright, & Colder (2013)"), URL = c("https://doi.org/10.1177/1073191111411672", 
      "https://doi.org/10.1177/1073191111411672", "https://doi.org/10.1177/1073191111411672"
      )), class = "data.frame", row.names = c(NA, -3L)))
    ,
    Scales = 
      structure(list(Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"
      ), Angle = c(90, 135, 180, 225, 270, 315, 360, 45), Items = c("1,  9, 17, 25", 
      "2, 10, 18, 26", "3, 11, 19, 27", "4, 12, 20, 28", "5, 13, 21, 29", "6, 14, 22, 30", 
      "7, 15, 23, 31", "8, 16, 24, 32"), Label = c("+A", "+A-C", "-C", "-A-C", 
      "-A", "-A+C", "+C", "+A+C")), class = "data.frame", row.names = c(NA, -8L
      ))
  ),
  iipsc = list(
    Norms = 
      list(structure(list(Sample = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 
      2, 2), Scale = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO", "PA", 
      "BC", "DE", "FG", "HI", "JK", "LM", "NO"), Angle = c(90, 135, 180, 225, 
      270, 315, 360, 45, 90, 135, 180, 225, 270, 315, 360, 45), M = c(0.76, 0.7925, 
      0.9, 1.0475, 1.42, 1.385, 1.465, 1.025, 0.99, 0.97, 1.3, 1.33, 1.81, 1.92, 
      2.14, 1.43), SD = c(0.66, 0.69, 0.855, 0.9475, 0.915, 0.8525, 0.825, 0.8, 
      0.82, 0.85, 1.07, 0.98, 0.89, 0.89, 0.9, 1.05)), class = "data.frame", row.names = c(NA, 
      -16L)), structure(list(Sample = c(1, 2), Size = c(872, 106), Population = c("American college students", 
      "American psychiatric outpatients"), Reference = c("Hopwood, Pincus, DeMoor, & Koonce (2008)", 
      "Soldz, Budman, Demby, & Merry (1995)"), URL = c("https://doi.org/10.1080/00223890802388665", 
      "https://doi.org/10.1177/1073191195002001006")), class = "data.frame", row.names = c(NA, 
      -2L)))
    ,
    Scales = 
      structure(list(Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"
      ), Angle = c(90, 135, 180, 225, 270, 315, 360, 45), Items = c("1,  9, 17, 25", 
      "2, 10, 18, 26", "3, 11, 19, 27", "4, 12, 20, 28", "5, 13, 21, 29", "6, 14, 22, 30", 
      "7, 15, 23, 31", "8, 16, 24, 32"), Label = c("Domineering", "Vindictive", 
      "Cold", "Socially Avoidant", "Nonassertive", "Exploitable", "Overly Nurturant", 
      "Intrusive")), class = "data.frame", row.names = c(NA, -8L))
  ),
  iip32 = list(
    Norms = 
      list(structure(list(Sample = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 
      2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3), Scale = c("PA", "BC", 
      "DE", "FG", "HI", "JK", "LM", "NO", "PA", "BC", "DE", "FG", "HI", 
      "JK", "LM", "NO", "PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"
      ), Angle = c(90, 135, 180, 225, 270, 315, 360, 45, 90, 135, 180, 
      225, 270, 315, 360, 45, 90, 135, 180, 225, 270, 315, 360, 45), 
    M = c(0.5, 0.675, 0.675, 0.825, 1, 1.075, 1.075, 0.675, 0.45, 
    0.5, 0.675, 0.75, 1.075, 1.2, 1.175, 0.625, 0.575, 0.75, 
    0.75, 0.825, 0.925, 1, 0.95, 0.7), SD = c(0.625, 0.825, 0.925, 
    0.825, 0.825, 0.75, 0.825, 0.65, 0.625, 0.825, 0.825, 0.825, 
    0.925, 0.825, 0.825, 0.7, 0.625, 0.825, 0.925, 0.925, 0.825, 
    0.75, 0.825, 0.7)), class = "data.frame", row.names = c(NA, 
      -24L)), structure(list(Sample = c(1, 2, 3), Size = c(800, 400, 
      400), Population = c("American adults, national standardization sample, overall", 
      "American adults, national standardization sample, females", 
      "American adults, national standardization sample, males"), Reference = c("Horowitz, Alden, Wiggins, & Pincus (2003)", 
      "Horowitz, Alden, Wiggins, & Pincus (2003)", "Horowitz, Alden, Wiggins, & Pincus (2003)"
      ), URL = c("https://www.mindgarden.com/113-inventory-of-interpersonal-problems", 
      "https://www.mindgarden.com/113-inventory-of-interpersonal-problems", 
      "https://www.mindgarden.com/113-inventory-of-interpersonal-problems"
      )), class = "data.frame", row.names = c(NA, -3L)))
    ,
    Scales = 
      structure(list(Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", 
      "LM", "NO"), Angle = c(90, 135, 180, 225, 270, 315, 360, 45), 
    Items = c("22, 25, 28, 30", "14, 16, 17, 18", "10, 11, 13, 15", 
    "2, 5, 9, 19", "4, 6, 7, 12", "1, 8, 20, 31", "23, 26, 27, 32", 
    "3, 21, 24, 29"), Label = c("Domineering/Controlling", "Vindictive/Self-Centered", 
    "Cold/Distant", "Socially Inhibited", "Nonassertive", "Overly Accommodating", 
    "Self-Sacrificing", "Intrusive/Needy")), class = "data.frame", row.names = c(NA, 
      -8L))
  ),
  iip64 = list(
    Norms = 
      list(structure(list(Sample = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 
      2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3), Scale = c("PA", "BC", 
      "DE", "FG", "HI", "JK", "LM", "NO", "PA", "BC", "DE", "FG", "HI", 
      "JK", "LM", "NO", "PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"
      ), Angle = c(90, 135, 180, 225, 270, 315, 360, 45, 90, 135, 180, 
      225, 270, 315, 360, 45, 90, 135, 180, 225, 270, 315, 360, 45), 
    M = c(0.6125, 0.6625, 0.7125, 0.8125, 0.925, 0.975, 1.025, 
    0.7125, 0.5625, 0.6, 0.6375, 0.8, 1, 1.075, 1.1, 0.675, 0.6625, 
    0.725, 0.7875, 0.825, 0.85, 0.8875, 0.9625, 0.7375), SD = c(0.5625, 
    0.6375, 0.7375, 0.7125, 0.7625, 0.6625, 0.6875, 0.6, 0.5125, 
    0.6125, 0.7, 0.7125, 0.7625, 0.675, 0.6875, 0.575, 0.5875, 
    0.65, 0.7625, 0.7125, 0.7625, 0.6375, 0.675, 0.625)), class = "data.frame", row.names = c(NA, 
      -24L)), structure(list(Sample = c(1, 2, 3), Size = c(800, 400, 
      400), Population = c("American adults, national standardization sample, overall", 
      "American adults, national standardization sample, females", 
      "American adults, national standardization sample, males"), Reference = c("Horowitz, Alden, Wiggins, & Pincus (2003)", 
      "Horowitz, Alden, Wiggins, & Pincus (2003)", "Horowitz, Alden, Wiggins, & Pincus (2003)"
      ), URL = c("https://www.mindgarden.com/113-inventory-of-interpersonal-problems", 
      "https://www.mindgarden.com/113-inventory-of-interpersonal-problems", 
      "https://www.mindgarden.com/113-inventory-of-interpersonal-problems"
      )), class = "data.frame", row.names = c(NA, -3L)))
    ,
    Scales = 
      structure(list(Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", 
      "LM", "NO"), Angle = c(90, 135, 180, 225, 270, 315, 360, 45), 
    Items = c("17, 31, 44, 45, 50, 52, 57, 59", "1, 22, 24, 29, 32, 40, 56, 64", 
    "11, 15, 16, 20, 23, 27, 36, 60", "3, 7, 14, 18, 33, 35, 55, 62", 
    "5, 6, 8, 9, 12, 13, 19, 39", "2, 10, 25, 34, 38, 42, 53, 61", 
    "21, 28, 37, 46, 49, 51, 54, 63", "4, 26, 30, 41, 43, 47, 48, 58"
    ), Label = c("Domineering/Controlling", "Vindictive/Self-Centered", 
    "Cold/Distant", "Socially Inhibited", "Nonassertive", "Overly Accommodating", 
    "Self-Sacrificing", "Intrusive/Needy")), class = "data.frame", row.names = c(NA, 
      -8L))
  )
)

test_that("audited instruments' norms and scales match their verified sources (M72)", {
  for (inst in names(audited_objects)) {
    exp <- audited_objects[[inst]]
    obj <- get(inst)
    expect_equal(obj$Norms, exp$Norms, info = inst)
    expect_equal(obj$Scales, exp$Scales, info = inst)
  }
})

test_that("the two shipped copies of each scale angle agree (M72)", {
  # Scales$Angle and Norms[[1]]$Angle are independent copies of one fact;
  # nothing in the package makes them agree, so an edit to one can desync them.
  for (inst in names(audited_objects)) {
    obj <- get(inst)
    norms <- obj$Norms[[1]]
    scales <- obj$Scales
    key <- if ("Scale" %in% names(norms)) "Scale" else "Abbrev"
    j <- match(norms[[key]], scales$Abbrev)
    expect_false(anyNA(j), info = inst)
    expect_equal(norms$Angle %% 360, scales$Angle[j] %% 360, info = inst)
  }
})

test_that("shipped angles follow the LM = 360 convention (M72)", {
  # DESIGN.md IP2: degrees in the user API run in (0, 360] with LM = 360, never
  # 0. The audit compares against the source modulo 360, which cannot see this.
  for (inst in names(audited_objects)) {
    obj <- get(inst)
    for (a in list(obj$Norms[[1]]$Angle, obj$Scales$Angle)) {
      expect_false(anyNA(a), info = inst)
      expect_true(all(a > 0 & a <= 360), info = inst)
    }
  }
})

test_that("every shipped instrument's item key partitions its items (M74)", {
  # score() takes rowMeans over exactly the numbers in Scales$Items, so an item
  # missing from every key is silently never scored and an item named twice is
  # scored into two octants. Neither shows up in any count the audit reports:
  # the audit compares the key against the SOURCE, and four of the shipped
  # instruments have no published item map to compare against at all.
  #
  # This is the failure cais shipped from 2018 to M74. Its 37 items sit in
  # octant blocks of unequal size (sodano2006 Table 1, p. 322: 5/5/5/4/5/3/5/5),
  # but the key carried the "1, 9, 17, 25" eight-cycle template the package's
  # other 32-item instruments use, which fits only when every block holds four.
  # The template dropped items 33-37 from every scale and misassigned 30, 31
  # and 32 -- and the item ORDER in the same file was right all along, so the
  # file contradicted itself. Sweeping all fifteen rather than the audited
  # thirteen is deliberate: iip32 and iip64 have no source on the shelf yet and
  # would otherwise be checked by nothing.
  for (inst in shipped_instruments()) {
    obj <- get(inst)
    keyed <- unlist(lapply(strsplit(obj$Scales$Items, ",", fixed = TRUE),
                           function(x) as.integer(trimws(x))))
    expect_false(anyNA(keyed), info = inst)
    expect_equal(sort(keyed), seq_len(obj$Details$Items), info = inst)
  }
})

test_that("cais's item key matches sodano2006's octant blocks (M74)", {
  # The partition test above catches a key that drops or repeats an item; this
  # one catches a key that partitions cleanly into the WRONG octants, which the
  # pre-M74 cais key also did (item 30 is an LM item, 31 is NO and 32 is PA).
  # Values are sodano2006 Table 1, p. 322, read in two channels; see
  # cairn/references/sodano2006.md.
  expect_equal(
    setNames(cais$Scales$Items, cais$Scales$Abbrev),
    c(PA = "1,  9, 17, 25, 32", BC = "2, 10, 18, 26, 33",
      DE = "3, 11, 19, 27, 34", FG = "4, 12, 20, 28",
      HI = "5, 13, 21, 29, 35", JK = "6, 14, 22",
      LM = "7, 15, 23, 30, 36", NO = "8, 16, 24, 31, 37")
  )
})

test_that("both IIP help pages carry the publisher's credit line (M75)", {
  # The Mind Garden permission licenses the IIP means and SDs on condition (a):
  # the credit line appears on the same page as the reproduced material. The
  # shipped norms ARE that material, so losing this line from either help page
  # is a permission breach, not a documentation nit.
  #
  # Read man/ when present and fall back to the installed Rd database, so this
  # runs both from a source checkout and against an installed package (the
  # M7/M70 dual-source pattern) rather than skipping in one of them.
  credit <- paste(
    "Reproduction by special permission of the Publisher, Mind Garden, Inc.,",
    "www.mindgarden.com from the Inventory of Interpersonal Problems by",
    "Leonard M. Horowitz, Lynn E. Alden, Jerry S. Wiggins, & Aaron L. Pincus.",
    "Copyright © 2000 by Leonard M. Horowitz, Lynn E. Alden,",
    "Jerry S. Wiggins, & Aaron L. Pincus. Further Reproduction is prohibited",
    "without the Publisher's written consent."
  )

  # Rd wraps the line across source lines, so compare on collapsed whitespace.
  # Comparing raw text would fail on a reflow that changed nothing.
  squash <- function(x) gsub("\\s+", " ", trimws(paste(x, collapse = " ")))

  rd_text <- function(topic) {
    f <- testthat::test_path("..", "..", "man", paste0(topic, ".Rd"))
    if (file.exists(f)) return(readLines(f, warn = FALSE))
    db <- tools::Rd_db("circumplex")
    nm <- paste0(topic, ".Rd")
    expect_true(nm %in% names(db))
    utils::capture.output(print(db[[nm]]))
  }

  for (topic in c("iip32", "iip64")) {
    expect_true(grepl(squash(credit), squash(rd_text(topic)), fixed = TRUE),
                info = paste(topic, "help page is missing the credit line"))
  }
})

test_that("the two IIP instruments ship an Items placeholder, not item text (M75)", {
  # The same permission covers the means and SDs ONLY, so the item TEXT of both
  # IIP forms is unlicensed and must not ship. The item NUMBERS in Scales$Items
  # are the scoring key and are permitted -- pinned by audited_objects above --
  # so this asserts the narrow thing: $Items stays a single pointer row with no
  # numbered item, which is what would change if item text were ever pasted in.
  for (inst in list(iip32, iip64)) {
    expect_identical(nrow(inst$Items), 1L)
    expect_true(is.na(inst$Items$Number))
    expect_match(inst$Items$Text, "Mind Garden")
    expect_false(grepl("^I ", inst$Items$Text))
  }
})

test_that("every audited instrument is a shipped instrument (M72)", {
  # Runtime half: works against the installed package, so it runs on CRAN.
  expect_true(all(names(audited_objects) %in% shipped_instruments()))
})

test_that("norms-audit.md's verdicts and the pin list bind in both directions (M73)", {
  # DEVELOPMENT-ONLY, same reasoning as the status-table test below: cairn/ is
  # repo tracking and is not installed. Split out so the runtime pins above
  # never skip with it.
  #
  # The two records this binds are written at different times by different
  # hands -- the verdict when a source note is authored, the pin when the
  # object is locked -- so either can move without the other. An instrument
  # whose verdict says it was audited but which nothing pins ships unguarded
  # while the tracking file claims otherwise; an instrument pinned while its
  # verdict still reads `unaudited` claims a guard the audit never established.
  # Assert the IFF, not either implication: a one-directional check passes over
  # exactly the half it does not look at.
  status <- testthat::test_path("..", "..", "cairn", "references",
                                "norms-audit.md")
  skip_if_not(file.exists(status), "cairn/ not present (installed package)")

  lines <- readLines(status, warn = FALSE)
  starts <- grep("^## ", lines)
  from <- grep("^## Audit status", lines)
  expect_length(from, 1L)
  to <- c(starts[starts > from], length(lines) + 1L)[[1]]
  section <- lines[(from + 1L):(to - 1L)]

  verdict_of <- function(inst) {
    row <- section[grepl(paste0("^\\| ", inst, " \\|"), section)]
    if (length(row) != 1L) return(NA_character_)
    trimws(strsplit(row, "|", fixed = TRUE)[[1]][[3]])
  }
  verdicts <- vapply(shipped_instruments(), verdict_of, character(1))
  expect_false(anyNA(verdicts))

  # An `unaudited` verdict is the only one that claims no audit; every other
  # wording -- `verified: ...`, `audited, norms unsourced: ...` -- claims one.
  claims_audit <- !grepl("^unaudited", verdicts)
  expect_setequal(names(verdicts)[claims_audit], names(audited_objects))
})

test_that("stamp_ledger() stamps a zero-row ledger as it does a one-row one (M73)", {
  # DEVELOPMENT-ONLY: data-raw/ is not installed, and unlike Rd or vignettes it
  # has no installed counterpart to read instead, so a skip is the legitimate
  # case the M70 lesson leaves open rather than the false coverage it warns
  # about. It is its own test so the runtime pins above never skip with it.
  script <- testthat::test_path("..", "..", "data-raw", "audit-norms.R")
  skip_if_not(file.exists(script), "data-raw/ not present (installed package)")

  # Base R rather than withr::with_options: withr is not in Suggests, and a
  # test file is not the place to acquire a dependency.
  env <- new.env()
  old <- options(norms_audit_defs_only = TRUE)
  on.exit(options(old), add = TRUE)
  sys.source(script, env)
  stamp_ledger <- get("stamp_ledger", envir = env)
  empty_ledger <- get("empty_ledger", envir = env)

  zero <- empty_ledger()
  one <- empty_ledger()
  one[1L, ] <- as.list(rep("x", ncol(one)))

  # The defect this fences: `df$col <- <scalar>` errors on a zero-row frame
  # ("replacement has 1 row, data has 0"), so the audit crashed on exactly the
  # clean ledger it exists to produce. Assert the arity that used to fail, and
  # assert it by NAME rather than by "no error" -- a stamped frame that dropped
  # a column would otherwise pass.
  expect_no_error(zs <- stamp_ledger(zero, generated = "2026-08-06"))
  expect_no_error(os <- stamp_ledger(one, generated = "2026-08-06"))
  expect_identical(nrow(zs), 0L)
  expect_identical(nrow(os), 1L)
  expect_identical(names(zs), names(os))
  expect_identical(names(zs), c(names(zero), "generated", "script_commit",
                                "data_commit"))
  expect_identical(os$generated, "2026-08-06")
  expect_identical(zs$generated, character(0))
})

test_that("a source note backing two instruments is read per instrument (M75)", {
  # DEVELOPMENT-ONLY, same reasoning as the stamp_ledger test above: the script
  # under test lives in data-raw/, which is not installed.
  script <- testthat::test_path("..", "..", "data-raw", "audit-norms.R")
  skip_if_not(file.exists(script), "data-raw/ not present (installed package)")

  env <- new.env()
  old <- options(norms_audit_defs_only = TRUE)
  on.exit(options(old), add = TRUE)
  sys.source(script, env)
  parse_source_note <- get("parse_source_note", envir = env)

  # The IIP manual is one source for two instruments whose rows key alike --
  # both have samples 1-3 over the same eight octant names -- so the note tags
  # its blocks. The fixture reproduces that shape rather than reading the real
  # note, so the test states the contract and does not restate the manual.
  dir <- tempfile("m75-notes-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  block <- function(tag, value) {
    c(paste0("<!-- audit-values-begin: ", tag, " -->"),
      "| field | sample | scale | value | anchor |",
      "| --- | --- | --- | --- | --- |",
      paste0("| M | 1 | PA | ", value, " | Table 1 |"),
      "<!-- audit-values-end -->")
  }
  writeLines(c("# two", block("aa", "1.5"), "", block("bb", "9.5")),
             file.path(dir, "two.md"))
  writeLines(c("# one", "<!-- audit-values-begin -->",
               "| field | sample | scale | value | anchor |",
               "| --- | --- | --- | --- | --- |",
               "| M | 1 | PA | 4.25 | Table 1 |",
               "<!-- audit-values-end -->"),
             file.path(dir, "one.md"))

  # Each instrument gets ITS OWN block's value. Assert the values, not merely
  # that a frame came back: the defect this fences is a note whose second block
  # is never reached, and a first-block fallback returns a perfectly well-formed
  # frame of the wrong instrument's numbers.
  expect_identical(parse_source_note("two", dir, "aa")$value, "1.5")
  expect_identical(parse_source_note("two", dir, "bb")$value, "9.5")
  expect_identical(attr(parse_source_note("two", dir, "bb"), "tag"), "bb")

  # An instrument no block names must abort. Assert the condition by its
  # message, not by "an error happened": a typo'd fixture path errors too.
  expect_error(parse_source_note("two", dir, "cc"),
               "no audit-values block for cc")
  expect_error(parse_source_note("two", dir), "no audit-values block for")

  # The untagged single-block note is the batch-1..3 shape and keeps working
  # for any instrument, with an empty tag.
  expect_identical(parse_source_note("one", dir, "anything")$value, "4.25")
  expect_identical(parse_source_note("one", dir)$value, "4.25")
  expect_identical(attr(parse_source_note("one", dir), "tag"), "")
})

test_that("norms-audit.md lists every shipped instrument (M72)", {
  # DEVELOPMENT-ONLY half: cairn/ is repo tracking, not installed, so this
  # cannot run under R CMD check. It is split out from the runtime assertions
  # above deliberately -- a whole-test skip here would silently take the pins
  # with it (the M70 lesson).
  status <- testthat::test_path("..", "..", "cairn", "references", "norms-audit.md")
  skip_if_not(file.exists(status), "cairn/ not present (installed package)")

  lines <- readLines(status, warn = FALSE)
  # Scoped to the status table's own section: every audited instrument also
  # appears in the citekey map further down, so a file-wide search passes over
  # a deleted status row.
  starts <- grep("^## ", lines)
  from <- grep("^## Audit status", lines)
  expect_length(from, 1L)
  to <- c(starts[starts > from], length(lines) + 1L)[[1]]
  section <- lines[(from + 1L):(to - 1L)]

  for (inst in shipped_instruments()) {
    expect_true(
      any(grepl(paste0("^\\| ", inst, " \\|"), section)),
      info = paste(inst, "missing from norms-audit.md status table")
    )
  }
})

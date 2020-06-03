context("test-instrument_oop")


test_that("S3 class constructors for instrument class work as expected", {
  i <- new_instrument(list(), list(), list(), list(), list())
  expect_s3_class(i, "circumplex_instrument")
  expect_equal(is_instrument(i), TRUE)
})


test_that("The instrument function works with string and NSE input", {
  isc <- instrument(isc)
  isc2 <- instrument("isc")
  expect_equal(isc, isc2)
})


test_that("The instrument function produces the same output as the data function", {
  by_instrument <- instrument("isc")
  data("isc")
  expect_equal(by_instrument, isc)
})


test_that("The print method for the S3 instrument class produces the right output", {
  isc <- instrument("isc")
  expect_output(
    print(isc),
    "ISC: Interpersonal Sensitivities Circumplex\\n64 items, 8 scales, 1 normative data sets\\nHopwood et al\\. \\(2011\\)\\n<https://doi\\.org/10\\.1111/j\\.1467-6494\\.2011\\.00696\\.x>"
  )
})


test_that("The summary method for the S3 instrument class produces the right output", {
  isc <- instrument("isc")
  expect_output(
    summary(isc, scales = FALSE, anchors = FALSE, items = FALSE, norms = FALSE),
    "ISC: Interpersonal Sensitivities Circumplex\\n64 items, 8 scales, 1 normative data sets\\nHopwood et al\\. \\(2011\\)\\n<https://doi\\.org/10\\.1111/j\\.1467-6494\\.2011\\.00696\\.x>"
  )
  expect_output(
    summary(isc, scales = TRUE, anchors = FALSE, items = FALSE, norms = FALSE),
    "ISC: Interpersonal Sensitivities Circumplex\\n64 items, 8 scales, 1 normative data sets\\nHopwood et al\\. \\(2011\\)\\n<https://doi\\.org/10\\.1111/j\\.1467-6494\\.2011\\.00696\\.x>\\n\\nThe ISC contains 8 circumplex scales\\.\\nPA: Sensitive to Control \\(90 degrees\\)\\nBC: Sensitive to Antagonism \\(135 degrees\\)\\nDE: Sensitive to Remoteness \\(180 degrees\\)\\nFG: Sensitive to Timidity \\(225 degrees\\)\\nHI: Sensitive to Passivity \\(270 degrees\\)\\nJK: Sensitive to Dependence \\(315 degrees\\)\\nLM: Sensitive to Affection \\(360 degrees\\)\\nNO: Sensitive to Attention-Seeking \\(45 degrees\\)"
  )
  expect_output(
    summary(isc, scales = FALSE, anchors = TRUE, items = FALSE, norms = FALSE),
    "ISC: Interpersonal Sensitivities Circumplex\\n64 items, 8 scales, 1 normative data sets\\nHopwood et al\\. \\(2011\\)\\n<https://doi\\.org/10\\.1111/j\\.1467-6494\\.2011\\.00696\\.x>\\n\\nThe ISC is rated using the following 8-point scale\\.\\n1\\. Not at all, never bothers me\\n2\\. Very little, rarely bothers me\\n3\\. A little, occasionally bothers me\\n4\\. Slightly, bothers me less than half the time\\n5\\. Somewhat, bothers me more than half of the time\\n6\\. Quite, bothers me frequently\\n7\\. Very much, bothers me most of the time\\n8\\. Extremely, always bothers me"
  )
  expect_output(
    summary(isc, scales = FALSE, anchors = FALSE, items = TRUE, norms = FALSE),
    "ISC: Interpersonal Sensitivities Circumplex\\n64 items, 8 scales, 1 normative data sets\\nHopwood et al\\. \\(2011\\)\\n<https://doi\\.org/10\\.1111/j\\.1467-6494\\.2011\\.00696\\.x>\\n\\nThe ISC contains 64 items \\(open-access\\):\\n1\\. Thinks they are my boss\\n2\\. Disregards my feelings\\n3\\. Doesn't show any feelings\\n4\\. Won't engage in conversation\\n5\\. Cannot assert themselves\\n6\\. Avoids conflict at all costs\\n7\\. Wants to spend lots of time with me\\n8\\. Doesn't respect my privacy\\n9\\. Orders me around\\n10\\. Doesn't respond to me\\n11\\. Doesn't want to be friends\\n12\\. Is aloof\\n13\\. Doesn't have a backbone\\n14\\. Always needs support\\n15\\. Tries to get close to me really fast\\n16\\. Talks themselves up\\n17\\. Talks down to me\\n18\\. Is hostile\\n19\\. Walls themselves off from me\\n20\\. Acts like a wallflower\\n21\\. Is weak\\n22\\. Always seems to need my help\\n23\\. Tells me they love me\\n24\\. Shows off\\n25\\. Will do anything to get what they want\\n26\\. Is deceitful\\n27\\. Won't share their feelings with me\\n28\\. Won't step up to the plate\\n29\\. Allows themselves to be dominated by others\\n30\\. Acts helpless\\n31\\. Expresses concern about me\\n32\\. Talks about themselves\\n33\\. Has to call the shots\\n34\\. Mistrusts me\\n35\\. Is unresponsive\\n36\\. Doesn't share their ideas\\n37\\. Is always submissive\\n38\\. Laughs at all my jokes\\n39\\. Acts like we're friends when we don't even know each other\\n40\\. Has to be right\\n41\\. Intimidates me\\n42\\. Doesn't care about my feelings\\n43\\. Avoids me\\n44\\. Won't participate\\n45\\. Cannot make decisions\\n46\\. Is dependent on me\\n47\\. Humors me\\n48\\. Interrupts\\n49\\. Is bossy\\n50\\. Expects the worst out of me\\n51\\. Never gets in touch with me\\n52\\. Is really shy\\n53\\. Is very passive\\n54\\. Believes everything I say\\n55\\. Believes I can do no wrong\\n56\\. Tries to show me how to do things\\n57\\. Always puts themselves first\\n58\\. Is mean-spirited\\n59\\. Pulls away from me\\n60\\. Refuses to lead\\n61\\. Is easily controlled\\n62\\. Is soft\\n63\\. Is clingy\\n64\\. Takes control"
  )
  expect_output(
    summary(isc, scales = FALSE, anchors = FALSE, items = FALSE, norms = TRUE),
    "ISC: Interpersonal Sensitivities Circumplex\\n64 items, 8 scales, 1 normative data sets\\nHopwood et al\\. \\(2011\\)\\n<https://doi\\.org/10\\.1111/j\\.1467-6494\\.2011\\.00696\\.x>\\n\\nThe ISC currently has 1 normative data set\\(s\\):\\n1\\. 649 American college students\\nHopwood et al\\. \\(2011\\)\\n<https://doi\\.org/10\\.1111/j\\.1467-6494\\.2011\\.00696\\.x>"
  )
})


test_that("The sub-summary functions produce the expected output", {
  isc <- instrument("isc")
  expect_output(
    scales(isc),
    "The ISC contains 8 circumplex scales\\.\\nPA: Sensitive to Control \\(90 degrees\\)\\nBC: Sensitive to Antagonism \\(135 degrees\\)\\nDE: Sensitive to Remoteness \\(180 degrees\\)\\nFG: Sensitive to Timidity \\(225 degrees\\)\\nHI: Sensitive to Passivity \\(270 degrees\\)\\nJK: Sensitive to Dependence \\(315 degrees\\)\\nLM: Sensitive to Affection \\(360 degrees\\)\\nNO: Sensitive to Attention-Seeking \\(45 degrees\\)"
  )
  expect_output(
    scales(isc, items = TRUE),
    "The ISC contains 8 circumplex scales\\.\\nPA: Sensitive to Control \\(90 degrees\\)\\n    1\\. Thinks they are my boss\\n    9\\. Orders me around\\n    17\\. Talks down to me\\n    25\\. Will do anything to get what they want\\n    33\\. Has to call the shots\\n    41\\. Intimidates me\\n    49\\. Is bossy\\n    57\\. Always puts themselves first\\nBC: Sensitive to Antagonism \\(135 degrees\\)\\n    2\\. Disregards my feelings\\n    10\\. Doesn't respond to me\\n    18\\. Is hostile\\n    26\\. Is deceitful\\n    34\\. Mistrusts me\\n    42\\. Doesn't care about my feelings\\n    50\\. Expects the worst out of me\\n    58\\. Is mean-spirited\\nDE: Sensitive to Remoteness \\(180 degrees\\)\\n    3\\. Doesn't show any feelings\\n    11\\. Doesn't want to be friends\\n    19\\. Walls themselves off from me\\n    27\\. Won't share their feelings with me\\n    35\\. Is unresponsive\\n    43\\. Avoids me\\n    51\\. Never gets in touch with me\\n    59\\. Pulls away from me\\nFG: Sensitive to Timidity \\(225 degrees\\)\\n    4\\. Won't engage in conversation\\n    12\\. Is aloof\\n    20\\. Acts like a wallflower\\n    28\\. Won't step up to the plate\\n    36\\. Doesn't share their ideas\\n    44\\. Won't participate\\n    52\\. Is really shy\\n    60\\. Refuses to lead\\nHI: Sensitive to Passivity \\(270 degrees\\)\\n    5\\. Cannot assert themselves\\n    13\\. Doesn't have a backbone\\n    21\\. Is weak\\n    29\\. Allows themselves to be dominated by others\\n    37\\. Is always submissive\\n    45\\. Cannot make decisions\\n    53\\. Is very passive\\n    61\\. Is easily controlled\\nJK: Sensitive to Dependence \\(315 degrees\\)\\n    6\\. Avoids conflict at all costs\\n    14\\. Always needs support\\n    22\\. Always seems to need my help\\n    30\\. Acts helpless\\n    38\\. Laughs at all my jokes\\n    46\\. Is dependent on me\\n    54\\. Believes everything I say\\n    62\\. Is soft\\nLM: Sensitive to Affection \\(360 degrees\\)\\n    7\\. Wants to spend lots of time with me\\n    15\\. Tries to get close to me really fast\\n    23\\. Tells me they love me\\n    31\\. Expresses concern about me\\n    39\\. Acts like we're friends when we don't even know each other\\n    47\\. Humors me\\n    55\\. Believes I can do no wrong\\n    63\\. Is clingy\\nNO: Sensitive to Attention-Seeking \\(45 degrees\\)\\n    8\\. Doesn't respect my privacy\\n    16\\. Talks themselves up\\n    24\\. Shows off\\n    32\\. Talks about themselves\\n    40\\. Has to be right\\n    48\\. Interrupts\\n    56\\. Tries to show me how to do things\\n    64\\. Takes control"
  )
  expect_output(
    items(isc),
    "The ISC contains 64 items \\(open-access\\):\\n1\\. Thinks they are my boss\\n2\\. Disregards my feelings\\n3\\. Doesn't show any feelings\\n4\\. Won't engage in conversation\\n5\\. Cannot assert themselves\\n6\\. Avoids conflict at all costs\\n7\\. Wants to spend lots of time with me\\n8\\. Doesn't respect my privacy\\n9\\. Orders me around\\n10\\. Doesn't respond to me\\n11\\. Doesn't want to be friends\\n12\\. Is aloof\\n13\\. Doesn't have a backbone\\n14\\. Always needs support\\n15\\. Tries to get close to me really fast\\n16\\. Talks themselves up\\n17\\. Talks down to me\\n18\\. Is hostile\\n19\\. Walls themselves off from me\\n20\\. Acts like a wallflower\\n21\\. Is weak\\n22\\. Always seems to need my help\\n23\\. Tells me they love me\\n24\\. Shows off\\n25\\. Will do anything to get what they want\\n26\\. Is deceitful\\n27\\. Won't share their feelings with me\\n28\\. Won't step up to the plate\\n29\\. Allows themselves to be dominated by others\\n30\\. Acts helpless\\n31\\. Expresses concern about me\\n32\\. Talks about themselves\\n33\\. Has to call the shots\\n34\\. Mistrusts me\\n35\\. Is unresponsive\\n36\\. Doesn't share their ideas\\n37\\. Is always submissive\\n38\\. Laughs at all my jokes\\n39\\. Acts like we're friends when we don't even know each other\\n40\\. Has to be right\\n41\\. Intimidates me\\n42\\. Doesn't care about my feelings\\n43\\. Avoids me\\n44\\. Won't participate\\n45\\. Cannot make decisions\\n46\\. Is dependent on me\\n47\\. Humors me\\n48\\. Interrupts\\n49\\. Is bossy\\n50\\. Expects the worst out of me\\n51\\. Never gets in touch with me\\n52\\. Is really shy\\n53\\. Is very passive\\n54\\. Believes everything I say\\n55\\. Believes I can do no wrong\\n56\\. Tries to show me how to do things\\n57\\. Always puts themselves first\\n58\\. Is mean-spirited\\n59\\. Pulls away from me\\n60\\. Refuses to lead\\n61\\. Is easily controlled\\n62\\. Is soft\\n63\\. Is clingy\\n64\\. Takes control"
  )
  expect_output(
    anchors(isc),
    "The ISC is rated using the following 8-point scale\\.\\n1\\. Not at all, never bothers me\\n2\\. Very little, rarely bothers me\\n3\\. A little, occasionally bothers me\\n4\\. Slightly, bothers me less than half the time\\n5\\. Somewhat, bothers me more than half of the time\\n6\\. Quite, bothers me frequently\\n7\\. Very much, bothers me most of the time\\n8\\. Extremely, always bothers me"
  )
  expect_output(
    norms(isc),
    "The ISC currently has 1 normative data set\\(s\\):\\n1\\. 649 American college students\\nHopwood et al\\. \\(2011\\)\\n<https://doi\\.org/10\\.1111/j\\.1467-6494\\.2011\\.00696\\.x>"
  )
})


test_that("The norms function detects when no norms are available", {
  isc_drop <- instrument("isc")
  isc_drop$Norms[[2]] <- tibble::new_tibble(list(), nrow = 0)
  expect_output(
    norms(isc_drop),
    "The ISC currently has no normative data sets\\."
  )
})


test_that("The instruments function produces the expected output", {
  expect_output(
    instruments(),
    "The circumplex package currently includes 13 instruments:\\n 1\\. CSIE: Circumplex Scales of Interpersonal Efficacy \\(csie\\)\\n 2\\. CSIG: Circumplex Scales of Intergroup Goals \\(csig\\)\\n 3\\. CSIP: Circumplex Scales of Interpersonal Problems \\(csip\\)\\n 4\\. CSIV: Circumplex Scales of Interpersonal Values \\(csiv\\)\\n 5\\. IGI-CR: Interpersonal Goals Inventory for Children, Revised Version \\(igicr\\)\\n 6\\. IIP-32: Inventory of Interpersonal Problems, Brief Version \\(iip32\\)\\n 7\\. IIP-64: Inventory of Interpersonal Problems \\(iip64\\)\\n 8\\. IIP-SC: Inventory of Interpersonal Problems, Short Circumplex \\(iipsc\\)\\n 9\\. IIS-32: Inventory of Interpersonal Strengths, Brief Version \\(iis32\\)\\n 10\\. IIS-64: Inventory of Interpersonal Strengths \\(iis64\\)\\n 11\\. IIT-C: Inventory of Influence Tactics Circumplex \\(iitc\\)\\n 12\\. IPIP-IPC: IPIP Interpersonal Circumplex \\(ipipipc\\)\\n 13\\. ISC: Interpersonal Sensitivities Circumplex \\(isc\\)"
  )
})

## Quiet R CMD check about global variables
if(getRversion() >= "2.15.1")  utils::globalVariables(
  c("Group", "V5", ".row", "Contrast", "Measure", ".", "Difference", "Type",
    "a_est", "a_lci", "a_uci", "circ_dist", "d_est", "d_lci", "d_uci", "e_est",
    "e_lci", "e_uci", "est", "f_lci", "f_uci", "fit", "key", "label", "lci",
    "octants", "quartile", "uci", "value", "x_est", "x_lci", "x_uci", "y_est",
    "y_lci", "y_uci"))

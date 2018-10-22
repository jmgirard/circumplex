ssm_rowwise <- function(.data, scales, angles, prefix = "", suffix = "") {
  scales_en <- rlang::enquo(scales)
  assert_that(is_provided(.data), is_provided(angles))
  assert_that(is_enquo(!!scales_en))
  assert_that(is.string(prefix), is.string(suffix))
  
}

data("jz2017")

scores <- select(jz2017, PA:NO)
scores <- scores[1, ]
angles <- octants() %>% as_degree() %>% as_radian()

#SSM
params <- ssm_parameters(as.numeric(scores), angles)
elev <- params[[1]]
lov <- params[[2]]
dom <- params[[3]]

#Manual
lov2 <- .414 * (scores$LM + (.707 * (scores$JK + scores$NO)))
dom2 <- .414 * (scores$PA + (.707 * (scores$BC + scores$NO)))

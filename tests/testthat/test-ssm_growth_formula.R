# ssm_growth_formula(): the fixed joint model as engine formulas (M151) -------
#
# The expected strings are the model the coverage oracle validated
# (devel/m27-coverage-oracle.R), stated here by hand with time = "t" and
# id = "subject" so that a builder that ignored either argument would fail.
# No engine is installed or called: the pieces are formula objects and text.

dep <- function(f) paste(deparse(f, width.cutoff = 500L), collapse = "")

test_that("ssm_growth_formula returns the glmmTMB pieces", {
  gf <- ssm_growth_formula("glmmTMB", time = "t", id = "subject")
  expect_s3_class(gf, "circumplex_growth_formula")
  expect_type(gf, "list")
  expect_identical(names(gf), c("formula", "dispformula"))
  expect_true(all(vapply(gf, inherits, logical(1), "formula")))
  expect_identical(dep(gf$formula),
                   "value ~ 0 + dv + dv:t + us(0 + dv | subject)")
  expect_identical(dep(gf$dispformula), "~0 + dv")
  expect_identical(attr(gf, "engine"), "glmmTMB")
})

test_that("ssm_growth_formula returns the nlme pieces", {
  gf <- ssm_growth_formula("nlme", time = "t", id = "subject")
  expect_s3_class(gf, "circumplex_growth_formula")
  expect_identical(names(gf), c("fixed", "random", "weights"))
  expect_true(all(vapply(gf, inherits, logical(1), "formula")))
  expect_identical(dep(gf$fixed), "value ~ 0 + dv + dv:t")
  expect_identical(dep(gf$random), "~0 + dv | subject")
  expect_identical(dep(gf$weights), "~1 | dv")
})

test_that("ssm_growth_formula returns the brms pieces", {
  gf <- ssm_growth_formula("brms", time = "t", id = "subject")
  expect_s3_class(gf, "circumplex_growth_formula")
  expect_identical(names(gf), c("formula", "sigma"))
  expect_true(all(vapply(gf, inherits, logical(1), "formula")))
  expect_identical(dep(gf$formula),
                   "value ~ 0 + dv + dv:t + (0 + dv | subject)")
  expect_identical(dep(gf$sigma), "sigma ~ 0 + dv")
})

test_that("ssm_growth_formula defaults to wave and person, glmmTMB first", {
  gf <- ssm_growth_formula()
  expect_identical(attr(gf, "engine"), "glmmTMB")
  expect_identical(dep(gf$formula),
                   "value ~ 0 + dv + dv:wave + us(0 + dv | person)")
})

test_that("ssm_growth_formula formulas carry no environment-bound state", {
  # A formula built from text at call time has the builder's frame as its
  # environment; the fit call must not depend on it, so the environment is
  # set to the global one and the pieces deparse identically across calls.
  a <- ssm_growth_formula("nlme", time = "t", id = "s")
  b <- ssm_growth_formula("nlme", time = "t", id = "s")
  expect_identical(lapply(a, dep), lapply(b, dep))
  for (f in a) expect_identical(environment(f), globalenv())
})

test_that("ssm_growth_formula refuses an engine outside the three", {
  expect_error(ssm_growth_formula("lme4"), "`engine`")
  expect_error(ssm_growth_formula(c("glmmTMB", "nlme")), "`engine`")
  expect_error(ssm_growth_formula(1), "`engine`")
})

test_that("ssm_growth_formula refuses a time or id that is not one name", {
  expect_error(ssm_growth_formula("glmmTMB", time = 1), "`time`")
  expect_error(ssm_growth_formula("glmmTMB", time = c("a", "b")), "`time`")
  expect_error(ssm_growth_formula("glmmTMB", id = 2), "`id`")
  expect_error(ssm_growth_formula("glmmTMB", time = ""), "`time`")
  expect_error(ssm_growth_formula("glmmTMB", time = NA_character_), "`time`")
})

test_that("ssm_growth_formula refuses time or id that collide with the model's own names", {
  # `dv` and `value` are the long table's coordinate and outcome columns, and
  # a time or id named after them would make the formula read the wrong
  # column without error.
  expect_error(ssm_growth_formula("glmmTMB", time = "dv"), "`time`")
  expect_error(ssm_growth_formula("glmmTMB", id = "value"), "`id`")
  expect_error(ssm_growth_formula("glmmTMB", time = "w", id = "w"), "`id`")
})

test_that("ssm_growth_formula pieces fit ssm_growth_data output by name", {
  # The names the formula reads (`value`, `dv`, time, id) are the columns
  # ssm_growth_data() writes under the same time and id, so model.matrix()
  # on the fixed part succeeds with no engine. Six columns: three dv
  # indicators and three dv:time slopes, the coefficient names the fixture
  # in tests/testthat/fixtures/growth-fixef.rds carries.
  data("simulated_growth")
  long <- ssm_growth_data(simulated_growth[1:30, ], PANO(),
                          id = "person", time = "wave")
  gf <- ssm_growth_formula("nlme", time = "wave", id = "person")
  X <- model.matrix(gf$fixed, data = long)
  expect_identical(colnames(X),
                   c("dve", "dvx", "dvy", "dve:wave", "dvx:wave", "dvy:wave"))
  expect_equal(nrow(X), nrow(long))
})

# ---- print(): the fit call to paste (AC3) -----------------------------------

test_that("print shows the fit call and the extraction line for each engine", {
  expect_snapshot(print(ssm_growth_formula("glmmTMB")))
  expect_snapshot(print(ssm_growth_formula("nlme")))
  expect_snapshot(print(ssm_growth_formula("brms")))
})

test_that("print returns its object invisibly", {
  gf <- ssm_growth_formula("nlme")
  res <- NULL
  capture.output(res <- withVisible(print(gf)))
  expect_false(res$visible)
  expect_identical(res$value, gf)
})

test_that("a non-syntactic time or id is backticked, never read as formula syntax", {
  # "wave + age" is one column name, not a covariate; "my wave" parses.
  gf <- ssm_growth_formula("glmmTMB", time = "wave + age", id = "my id")
  expect_identical(dep(gf$formula),
                   "value ~ 0 + dv + dv:`wave + age` + us(0 + dv | `my id`)")
  expect_identical(all.vars(gf$formula), c("value", "dv", "wave + age", "my id"))
  nl <- ssm_growth_formula("nlme", time = "my wave", id = "sub id")
  expect_identical(dep(nl$fixed), "value ~ 0 + dv + dv:`my wave`")
  expect_identical(dep(nl$random), "~0 + dv | `sub id`")
  # The backticked fixed part reads the long table built under the same names.
  data("simulated_growth")
  d <- simulated_growth[1:9, ]
  names(d)[names(d) == "wave"] <- "my wave"
  long <- ssm_growth_data(d, PANO(), id = "person", time = "my wave")
  X <- model.matrix(ssm_growth_formula("nlme", time = "my wave")$fixed,
                    data = long)
  expect_identical(colnames(X),
                   c("dve", "dvx", "dvy", "dve:`my wave`", "dvx:`my wave`",
                     "dvy:`my wave`"))
  # A syntactic name is left bare.
  expect_identical(dep(ssm_growth_formula("nlme", time = "t")$fixed),
                   "value ~ 0 + dv + dv:t")
})

test_that("the printed glmmTMB call equals the vignette's fit chunk", {
  # Copied once, verbatim, from the base commit's
  # vignettes/growth-ssm-analysis.Rmd.orig `fit` chunk (M151 AC3). The print
  # is the paste-ready form of this call; the chunk is the reference.
  vignette_chunk <- "fit <- glmmTMB::glmmTMB(
  value ~ 0 + dv + dv:wave + us(0 + dv | person),
  dispformula = ~ 0 + dv,
  data = long,
  REML = TRUE
)"
  lines <- capture.output(print(ssm_growth_formula("glmmTMB")))
  from <- which(startsWith(lines, "fit <- "))
  to <- which(lines == ")")
  expect_length(from, 1)
  expect_length(to, 1)
  printed_call <- paste(lines[from:to], collapse = "\n")
  strip <- function(s) gsub("[[:space:]]+", "", s)
  expect_identical(strip(printed_call), strip(vignette_chunk))
})

test_that("the printed call uses the given time and id", {
  lines <- capture.output(print(ssm_growth_formula("nlme", time = "t",
                                                   id = "subject")))
  expect_true(any(grepl("dv:t,", lines, fixed = TRUE)))
  expect_true(any(grepl("| subject,", lines, fixed = TRUE)))
  expect_true(any(lines == "coef <- nlme::fixef(fit)"))
  lines <- capture.output(print(ssm_growth_formula("brms")))
  expect_true(any(lines == "draws <- as.matrix(fit)"))
})

# M54 axes_reliability(): fixed axis weights and per-axis item_n (T1).

test_that("BC3: per-axis item_n is exact for balanced octant instruments", {
  ang <- octants()
  expect_identical(axis_item_n(ang, 8L), c(x = 32, y = 32)) # 64-item
  expect_identical(axis_item_n(ang, 4L), c(x = 16, y = 16)) # 32-item
  expect_identical(axis_item_n(ang, 2L), c(x = 8, y = 8))   # 16-item
  # equal across the two axes for every balanced octant instrument
  for (k in 1:8) {
    inn <- axis_item_n(ang, k)
    expect_identical(inn[["x"]], inn[["y"]])
  }
})

test_that("BC10: pole weights snap exactly and theta 0 == 360", {
  expect_identical(as.numeric(axis_weights(360)), c(1, 0)) # LM at the pole
  expect_identical(as.numeric(axis_weights(90)), c(0, 1))  # PA on the y-axis
  expect_identical(axis_weights(0), axis_weights(360))     # 0 and 360 coincide
  # no ~1e-16 residue leaks: off-pole weights are exactly +/- cos(45 deg)
  w <- axis_weights(octants())
  resid <- w[!(w %in% c(0, 1, -1))]
  expect_true(all(abs(abs(resid) - cospi(0.25)) < 1e-12))
})

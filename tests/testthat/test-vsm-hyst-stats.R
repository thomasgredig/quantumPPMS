test_that("check coercivity from hysteresis loop", {
  filename = vsm.getSampleFiles()[1]
  d = vsm.import(filename)
  a = vsm.hystStatsLoop(d)

  expect_equal(a$Mrem, -39e-6, tolerance = 1e-3)
  expect_equal(a$Hc, 413, tolerance = 5e-3)
})

test_that("test correction of hysteresis loop data", {
  filename = vsm.getSampleFiles()[1]
  d = vsm.import(filename, dataFrame=TRUE)
  d$Mcorr = get.Mcorr(d)
  expect_equal( sum( d$Mcorr), -0.0003362446)
})

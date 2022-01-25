test_that("check coercivity from hysteresis loop", {
  filename = vsm.getSampleFiles()[1]
  d = vsm.import(filename)
  a = vsm.hystStats(d)

  expect_equal(a$Mrem, -39e-6, tolerance = 1e-3)
  expect_equal(a$Hc, 413, tolerance = 1e-3)
})

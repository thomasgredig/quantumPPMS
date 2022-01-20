context("VSM hysteresis statistics")

test_that("check coercivity from hysteresis loop", {
  filename = vsm.getSampleFiles()[1]
  d = ppms.load(filename)
  h = vsm.get.HystLoops(d)
  analys = vsm.hyst.stats(h)

  expect_equal(signif(analys$Hc,3),-112)

})

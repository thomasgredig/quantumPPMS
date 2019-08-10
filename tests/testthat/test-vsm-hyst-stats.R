context("VSM hysteresis statistics")

test_that("check coercivity from hysteresis loop", {
  library(plyr)
  filename = system.file("extdata", "20170620_BITHERMAL_SF_VSM_SF170517SI2_MVSH_3K.DAT", package="quantumPPMS")
  d = ppms.load(filename)
  h = vsm.get.HystLoops(d)
  analys = vsm.hyst.stats(h)

  expect_equal(signif(analys$Hc,3),-112)

})

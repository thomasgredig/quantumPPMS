context("VSM hysteresis statistics")

test_that("check coercivity from hysteresis loop", {
  path.source = '' #../..'

  #filename = dir(path.source,pattern='DAT$', recursive=TRUE)[1]
  filename = system.file("extdata", "20170620_BITHERMAL_SF_VSM_SF170517SI2_MVSH_3K.DAT", package="quantumPPMS")
  #expect_equal(filename, 'inst/extdata/20170620_BITHERMAL_SF_VSM_SF20170517SI2_MVSH_3K.DAT')

  d = ppms.load(file.path(path.source,filename))


})

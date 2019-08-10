context("loading Quantum Design PPMS file")

test_that("loading sample PPMS data file", {

  #filename = file.path('../..',dir('../..',pattern='DAT$', recursive=TRUE)[1])
  filename = system.file("extdata", "20170620_BITHERMAL_SF_VSM_SF170517SI2_MVSH_3K.DAT", package="quantumPPMS")
  expect_true(file.exists(filename))

  d = ppms.load(filename)
  expect_equal(nrow(d), 521)
})

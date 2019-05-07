context("loading Quantum Design PPMS file")

test_that("loading sample PPMS data file", {

  filename = file.path('../..',dir('../..',pattern='DAT$', recursive=TRUE)[1])
  expect_equal(length(filename),1)

  d = ppms.load(filename)
  expect_equal(nrow(d), 521)
})

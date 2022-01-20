test_that("loading sample PPMS data file", {
  filename = vsm.getSampleFiles()
  expect_true(file.exists(filename))

  d = ppms.load(filename)
  expect_equal(nrow(d), 520)
})


test_that("valid VSM file", {
  expect_true(vsm.validFile(vsm.getSampleFiles()))
})

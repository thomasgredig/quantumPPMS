test_that("loading sample VSM data file", {
  filename = vsm.getSampleFiles()[1]
  expect_true(file.exists(filename))

  d = vsm.import(filename)
  expect_equal(summary(d)$dataPoints, 520)
})


test_that("valid VSM file", {
  expect_true(vsm.validFile(vsm.getSampleFiles()[1]))
})


test_that("get first loop", {
  filename = vsm.getSampleFiles()[1]
  d = vsm.import(filename)

  d2 = vsm.getLoop(d)
  expect_equal(summary(d2)$dataPoints, 263)

  d2 = vsm.getLoop(d, direction = -1)
  expect_equal(summary(d2)$dataPoints, 257)
})

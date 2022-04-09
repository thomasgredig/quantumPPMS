test_that("loading sample VSM data file", {
  filename = vsm.getSampleFiles()[1]
  expect_true(file.exists(filename))

  d = vsm.import(filename)
  expect_equal(summary(d)$dataPoints, 520)
})


test_that("valid VSM file", {
  expect_true(vsm.validFile(vsm.getSampleFiles()[1]))
})

test_that("test loading empty file", {
  filename = vsm.getSampleFiles(type='empty')
  expect_warning(vsm.import(filename))
})


test_that("get first loop", {
  filename = vsm.getSampleFiles()[1]
  d = vsm.import(filename)

  d2 = vsm.getLoop(d)
  expect_equal(summary(d2)$dataPoints, 263)

  d2 = vsm.getLoop(d, direction = -1)
  expect_equal(summary(d2)$dataPoints, 257)
})


test_that("version checking for files", {
  file.list = vsm.getSampleFiles("version")
  v = 0
  for(f in file.list) {
    v = v + vsm.version(f,FALSE)
    expect_true(vsm.validFile(f))
  }
  expect_equal(v, 8.1884)
})


test_that("test loading different versions of VSM files", {
  file.list = vsm.getSampleFiles("version")

  for(f in file.list) {
    # attempt to import data
    vsm.import(f,dataFrame = TRUE, verbose=FALSE) -> d
    expect_true(nrow(d)>0)
  }
})


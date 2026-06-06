test_that("valid VMS temperatures", {
  file.list <- vsm.getSampleFiles()[c(1,3,5:8)]
  for (filename in file.list) {
    #print(filename)
    d <- vsm.import(filename, dataFrame = TRUE)
    expect_true(any(d$T > 0))
    expect_true(any(d$T < 1000))
    expect_true(any(d$time >= 0))
    expect_true(any(abs(d$H) < 10 * 1e4))
    expect_true(any(d$M < 1))
  }
})

test_that("check dataframe", {
  filename <- vsm.getSampleFiles()[5]
  d <- vsm.import(filename, dataFrame = FALSE)
  df <- vsm.data.frame(d)

  expect_true(any(df$T.K > 0))
})

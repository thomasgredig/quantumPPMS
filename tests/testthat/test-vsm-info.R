test_that("VSM info", {
  file.list = vsm.getSampleFiles()

  sn = c()
  for(f in file.list) {
    d = vsm.info(f)
    sn = c(sn, d$sample.name)
  }
  #print(paste(sn, collapse=","))
  expect_equal(paste(sn, collapse=","),
               "SF170517SI2,TS100928Si1,INFO, COMMENT, July 2006 FePc powder Sample,DATATYPE, COMMENT, 1,TS100928Si1,TS101218Au,TS100928Si1,TS100917Si1")
})



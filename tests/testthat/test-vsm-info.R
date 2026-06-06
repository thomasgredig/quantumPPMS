test_that("VSM info", {
  file.list = vsm.getSampleFiles()

  sn = c()
  for(f in file.list) {
    d = vsm.info(f)
    sn = c(sn, d$sample.name)
    #cat(">",d$sample.name,"\n")
  }
  #"SF20170517 S4,TS100928Si1, FePc powder in capsule, FePc(200nm)/Si, Deposited at 230C,TS100928Si1,TS101218Au,TS100928Si1,TS100917Si1")

  expect_equal(
    sn,
    c(
      "SF20170517 S4",
      "TS100928Si1, FePc(200nm)/Si, Deposited at 230C",
      "FePc powder in capsule",
      "",
      "TS100928Si1 ZFC AC susceptibility measurement",
      "TS101218Au, FePc 200nm, 180C",
      "TS100928Si1, FePc(200nm)/Si, Deposited at 230C",
      "FePc/Si"
    )
  )
})
# TS100928Si1,TS101218Au,TS100928Si1,TS100917Si1"


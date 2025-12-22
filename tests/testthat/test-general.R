test_that("VSM help", {
  vsm.help('ppms.vsm.hystLoops') -> text
  expect_true(nchar(text)>0)

  vsm.help() -> text
  expect_true(nchar(text)>0)
})

test_that("vsmdata class for temperature dependence", {
  d = VSMdata(
    time = 1:10,
    T = 200 + 1:10*5,
    H = rep(5,10),
    M = rep(1,10),
    Merr = rep(0.1,10)
  )
  expect_true(inherits(d, "VSMdata"))
  expect_true(d@type[1]=="MvsT")
})

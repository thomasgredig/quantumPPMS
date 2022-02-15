test_that("VSM help", {
  vsm.help('ppms.vsm.hystLoops') -> text
  expect_true(nchar(text)>0)

  vsm.help() -> text
  expect_true(nchar(text)>0)
})


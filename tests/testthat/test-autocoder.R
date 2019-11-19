library(testthat)
context("Testing the autocode function")
data(RS.data)
rs_text = RS.data$text

test_that("test autocoding code set", {
  code.data = create.code(name = "Data", expressions = c("number","data"), excerpts = rs_text)
  code.people = create.code(name = "People", expressions = c("people","he", "she", "they"), excerpts = rs_text)

  code.set = code.set("Demo RS CodeSet", "CodeSet made for the demo", codes = c(code.data, code.people))
  
  allcoded = autocode(x = code.set, excerpts = rs_text, simplify = TRUE)
  
  expect_equivalent(colnames(allcoded), c("ID", "excerpt", "Data", "People"))
  expect_equal(nrow(allcoded), length(rs_text))
  expect_true(all(allcoded$Data %in% c(0, 1)))
  expect_true(all(allcoded$People %in% c(0, 1)))
})

test_that("test autocoding single code", {
  code.data = create.code(name = "Data", expressions = c("number","data"), excerpts = rs_text)
  allcoded = autocode(x = code.data, excerpts = rs_text, simplify = TRUE)
  
  expect_equivalent(colnames(allcoded), c("ID", "excerpt", "Data"))
  expect_equal(nrow(allcoded), length(rs_text))
  expect_true(all(allcoded$Data %in% c(0, 1)))
})

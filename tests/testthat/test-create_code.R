library(magrittr)
data(RS.data)
rs_text = RS.data$text

test_that("Basic creation", {
  name = "Data"
  newcode = create.code(name, expressions = c("number","data"), excerpts = rs_text)
  testthat::expect_equal(newcode$name, name)
})
test_that("Verify holdout sets", {
  name <- "Data"
  newcode <- create.code(name, expressions = c("number","data"), excerpts = rs_text)

  touchable_perc <- length(newcode$touchableExcerpts) / length(newcode$excerpts)
  holdout_perc <- length(newcode$holdoutExcerpts) / length(newcode$excerpts)
  
  testthat::expect_equal(0.1, touchable_perc, tolerance = 0.01)
  testthat::expect_equal(0.9, holdout_perc, tolerance = 0.01)
})
test_that("Check the touched sets", {
  name <- "Data"
  set <- 10:15
  exprs <- c("number","priority")
  coded <- sapply(rs_text[set], grepl, pattern = paste0(exprs, collapse = "|")) * 1
  
  newcode <- create.code(name, expressions = exprs, excerpts = rs_text) %>%
              getHandSetIndices2()
  
  newcode2 <- getHandSetIndices2(newcode)

  testthat::expect_gt(length(newcode$touchedIndices), expected = 0)
  
  touchable_perc <- length(newcode$touchableExcerpts) / length(newcode$excerpts)
  holdout_perc <- length(newcode$holdoutExcerpts) / length(newcode$excerpts)
  
  testthat::expect_equal(0.1, touchable_perc, tolerance = 0.01)
  testthat::expect_equal(0.9, holdout_perc, tolerance = 0.01)
})

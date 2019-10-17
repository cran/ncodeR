data(RS.data)
rs_text = RS.data$text

test_that("code summary", {
  name <- "Data"
  set <- 10:15
  exprs <- c("number","priority")
  coded <- sapply(rs_text[set], grepl, pattern = paste0(exprs, collapse = "|")) * 1
  
  newcode <- create.code(name, expressions = exprs, excerpts = rs_text) %>% 
              handcode(this.set = set, results = 0) %>% 
              test()
  
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  sink(tmp)
  summary(newcode)
  
  summ_output <- readLines(tmp)
  unlink(tmp)
  sink(file = NULL)
  
  testthat::expect_false(
    any(grepl(pattern = "NULL", x = summ_output))
  )
})

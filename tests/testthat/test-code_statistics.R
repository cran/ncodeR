data(RS.data)
rs_text = RS.data$text

testthat::test_that("Verify old parameter", {
  name <- "Data"
  set <- 10:15
  exprs <- c("number","priority")
  coded <- sapply(rs_text[set], grepl, pattern = paste0(exprs, collapse = "|")) * 1
  
  newcode <- create.code(name, expressions = exprs, excerpts = rs_text) %>% 
              handcode(this.set = set, results = 0)

  testthat::expect_warning(test(code = newcode, kappaThreshold = 0.9))
})
testthat::test_that("Verify statistics output", {
  name <- "Data"
  set <- 10:15
  exprs <- c("number","priority")
  coded <- sapply(rs_text[set], grepl, pattern = paste0(exprs, collapse = "|")) * 1
  
  newcode <- create.code(name, expressions = exprs, excerpts = rs_text) %>% 
              handcode(this.set = set, results = 0) %>% 
              test()
  
  testthat::expect_equal(length(newcode$statistics), 1)
  
  first_stats <- newcode$statistics[[1]]
  
  testthat::expect_equal(first_stats$one_v_classifier$test_set$N, length(set))
  testthat::expect_equal(
    as.numeric(first_stats$one_v_classifier$test_set$set[,2]),
    coded
  )
})
testthat::test_that("Verify second rater stats", {
  name <- "Data"
  set <- 10:15
  second_set <- c(1, rep(x = 0, length(set) - 1))
  exprs <- c("number","priority")
  coded <- sapply(rs_text[set], grepl, pattern = paste0(exprs, collapse = "|")) * 1
  
  newcode <- create.code(name, expressions = exprs, excerpts = rs_text) %>% 
              handcode(this.set = set, results = 0)
  
  newcode$secondRaterSet <- newcode$computerSet
  newcode$secondRaterSet$Data <- second_set
  
  newcode <- suppressWarnings(test(newcode))
  
  gold_set <- newcode$statistics[[1]]$one_v_two$test_set$set[,2]
  silv_set <- newcode$statistics[[1]]$one_v_two$test_set$set[,3]
  
  testthat::expect_equal(gold_set, rep(0, length(set)))
  testthat::expect_equal(silv_set, second_set)
})
testthat::test_that("Verify stats history", {
  name <- "Data"
  set <- 10:15
  exprs <- c("number","priority")
  coded <- sapply(rs_text[set], grepl, pattern = paste0(exprs, collapse = "|")) * 1
  
  newcode <- create.code(name, expressions = exprs, excerpts = rs_text) %>% 
              handcode(this.set = set, results = 0) %>%
              test()
  
  newcode <- handcode(code = newcode, this.set = 200:205, results = 1)
  newcode <- suppressWarnings(newcode %>% test())
  
  first_stats <- newcode$statistics[[1]]
  second_stats <- newcode$statistics[[2]]
  expect_equal(length(newcode$statistics), 2)
})
testthat::test_that("Verify clearing of test set", {
  name <- "Data"
  set <- 10:15
  exprs <- c("number","priority")
  coded <- sapply(rs_text[set], grepl, pattern = paste0(exprs, collapse = "|")) * 1
  
  newcode <- create.code(name, expressions = exprs, excerpts = rs_text) %>% 
              handcode(this.set = set, results = 0)
  newcode <- suppressWarnings(newcode %>% test())
  
  first_stats <- newcode$statistics[[1]]
  expect_equal(nrow(first_stats$one_v_classifier$test_set$set), length(coded))
  expect_null(first_stats$one_v_classifier$training_set$set)

  newcode_w_training <- suppressWarnings(handcode(code = newcode, this.set = 200:205, results = 1) %>% test())
  
  second_stats <- newcode_w_training$statistics[[2]]
  
  expect_equal(newcode_w_training$trainingSet, newcode$testSet)
})
testthat::test_that("Calculate precision and recall", {
  name <- "Data"
  set <- 10:15
  exprs <- c("number","priority")
  coded <- sapply(rs_text[set], grepl, pattern = paste0(exprs, collapse = "|")) * 1
  
  newcode <- create.code(name, expressions = exprs, excerpts = rs_text) %>% 
              handcode(this.set = set, results = 0) %>% 
              test()
  testthat::expect_equal(newcode$statistics[[1]]$one_v_classifier$test_set$recall, 0)
  testthat::expect_true(is.nan(newcode$statistics[[1]]$one_v_classifier$test_set$precision))
  
  newcode2 <- create.code(name, expressions = exprs, excerpts = rs_text) %>% 
                handcode(this.set = set, results = c(rep(0, 5), 1)) %>% 
                test()
  testthat::expect_equal(newcode2$statistics[[1]]$one_v_classifier$test_set$recall, 1)
  testthat::expect_equal(newcode2$statistics[[1]]$one_v_classifier$test_set$precision, 1)
  
  newcode3 <- create.code(name, expressions = exprs, excerpts = rs_text) %>% 
                handcode(this.set = 10:30, results = c(rep(0, 5),1, rep(0,10),1,1,1,0,0) ) %>% 
                test()
  testthat::expect_equal(newcode3$statistics[[1]]$one_v_classifier$test_set$recall, 1)
  testthat::expect_equal(newcode3$statistics[[1]]$one_v_classifier$test_set$precision, 0.75)
  
  newcode4 <- create.code(name, expressions = exprs, excerpts = rs_text) %>% 
                handcode(this.set = 10:30, results = c(rep(0, 5),1, rep(0,10),1,0,0,0,0) ) %>% 
                test()
  testthat::expect_equal(round(newcode4$statistics[[1]]$one_v_classifier$test_set$recall, 2), 0.67)
  testthat::expect_equal(newcode4$statistics[[1]]$one_v_classifier$test_set$precision, 1)
})
data(RS.data)
rs_text = RS.data$text

test_that("Basic second rater", {
  name <- "Data"
  set <- 10:15
  exprs <- c("number","priority")
  coded <- sapply(rs_text[set], grepl, pattern = paste0(exprs, collapse = "|")) * 1
  
  newcode <- create.code(name, expressions = exprs, excerpts = rs_text) %>% 
              handcode(this.set = set, results = 0)

  newcode <- suppressWarnings(test(code = newcode, kappa_threshold = 0.9))
  testthat::expect_equal(length(newcode$statistics), 1)
  
  newcode$secondRaterSet <- data.frame(ID = newcode$testSet[,1], newcode$computerSet[,2])
  newcode <- suppressWarnings(test(code = newcode, kappa_threshold = 0.9))
  testthat::expect_equal(length(newcode$statistics), 2)
  testthat::expect_equal(newcode$statistics[[2]]$two_v_classifier$test_set$kappa, 1)
  testthat::expect_null(newcode$statistics[[2]]$one_v_two$training_set)
})


test_that("Second rater training set", {
  name <- "Data"
  set_one <- 10:15
  set_two <- 16:25
  exprs <- c("number","priority")
  coded <- sapply(rs_text[set_one], grepl, pattern = paste0(exprs, collapse = "|")) * 1
  
  newcode <- create.code(name, expressions = exprs, excerpts = rs_text) %>% 
              handcode(this.set = set_one, results = 0) %>%
              test(kappa_threshold = 0.9)
  
  newcode$secondRaterSet <- data.frame(ID = newcode$testSet[,1], newcode$computerSet[,2])
  newcode <- suppressWarnings(test(code = newcode, kappa_threshold = 0.9))
  
  newcode <- suppressWarnings(handcode(code = newcode, this.set = set_two, results = 0) %>%
              test(kappa_threshold = 0.9))
  
  testthat::expect_null(newcode$statistics[[3]]$one_v_two$test_set)
  testthat::expect_equal(newcode$statistics[[3]]$one_v_two$training_set$set$ID, set_one)
  newcode$secondRaterSet <- rbind(
    newcode$secondRaterSet,
    data.frame(ID = newcode$testSet[,1], "Data" = 1)
  )
  newcode <- suppressWarnings(test(code = newcode, kappa_threshold = 0.9))
  newcode$statistics[[4]]$one_v_two$test_set
  testthat::expect_false(is.null(newcode$statistics[[4]]$one_v_two$test_set))
  testthat::expect_equal(newcode$statistics[[4]]$one_v_two$training_set$set$ID, set_one)
  testthat::expect_equal(newcode$statistics[[4]]$one_v_two$test_set$set$ID, set_two)
})
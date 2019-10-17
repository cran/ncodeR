calc_statistics <- function(set, kappa_threshold = 0.9, baserate_inflation = 0.2) {
  stats = list( rho = NA, kappa = NA )

  if( nrow(set) > 0 ) {
    stats <- tryCatch(
      rhoR::rhoSet(
        set[, -1],
        ScSKappaThreshold = kappa_threshold,
        testSetBaserateInflation = baserate_inflation
      ),
      error = function(x) {
        warning("Unable to calculate valid rho, returning NA with kappa")
        list(rho = NA, kappa = rhoR::kappaSet(set[, -1]))
      }
    )
  }
  stats$N <- nrow(set)
  stats$set <- set
  
  stats
}

#' Title
#'
#' @param code [TBD]
#' @param kappa_threshold  [TBD]
#' @param baserate_inflation  [TBD]
#' @param ... [TBD]
#'
#' @return code object
#' @export
test <- function(code, kappa_threshold = 0.65, baserate_inflation = 0.2, ...) {
  args <- list(...) 
  if(!is.null(args$kappaThreshold)) {
    warning("Use kappa_threshold instead of kappaThreshold. kappaThreshold will be deprecated in a future version.")
    kappa_threshold <- args$kappaThreshold
  }
  code.to.use <- code$clone(deep = TRUE);
  code.to.use$baserateInflation <- baserate_inflation;
  code.to.use$computerSet[,2] = code.to.use$process(code.to.use$excerpts[code.to.use$computerSet[,1]])
  code.to.use$baserate <- mean(code.to.use$process(code.to.use$excerpts[code.to.use$touchedIndices]))

  first_v_classifier_test <- NULL
  first_v_classifier_train <- NULL
  first_v_second <- NULL
  second_v_classifier <- NULL
  first_v_second_train <- NULL
  second_v_classifier_train <- NULL
  if(nrow(code$testSet) > 0) {
    to.test <- merge(code.to.use$computerSet, code.to.use$testSet, by = 1)
    
    if(!any(is.na(to.test[, 2:3])))
      first_v_classifier_test = calc_statistics(to.test)
  }
  if(nrow(code.to.use$trainingSet) > 0) {
    to.test = merge(code.to.use$computerSet, code.to.use$trainingSet, by = 1)
    
    to.test[is.na(to.test[,3]), 3] = abs(to.test[is.na(to.test[,3]), 2] - 1)
    to.test[!to.test[,3] %in% 0:1, 3] <- abs(to.test[!to.test[,3] %in% 0:1, 2] - 1)
    first_v_classifier_train = calc_statistics(to.test)
  }
  
  first_v_classifier <- test_result(
    test_set = first_v_classifier_test,
    training_set = first_v_classifier_train
  )
  
  second_set_training_ids <- unique(as.numeric(unlist(sapply(code$statistics, function(s) {
    s$one_v_two$test_set$set$ID
  }))))
  second_set_training <- code.to.use$secondRaterSet[code.to.use$secondRaterSet[,1] %in% second_set_training_ids,]
  second_set_testing <- code.to.use$secondRaterSet[!code.to.use$secondRaterSet[,1] %in% second_set_training_ids,]
  if(nrow(second_set_testing) > 0) {
    first_set <- rbind(code.to.use$testSet, code.to.use$trainingSet)
    to.test <- merge(first_set, second_set_testing, by = 1, all = TRUE)
    to.test <- to.test[rowSums(!is.na(to.test[,2:3]) * 1) > 1, ]
    first_v_second <- calc_statistics(to.test)
    
    to.test <- merge(code.to.use$computerSet, second_set_testing, by = 1, all = TRUE)
    to.test <- to.test[rowSums(!is.na(to.test[,2:3]) * 1) > 1, ]
    second_v_classifier <- calc_statistics(to.test)
  }
  if(nrow(second_set_training) > 0) {
    first_set <- rbind(code.to.use$testSet, code.to.use$trainingSet)
    to.test <- merge(first_set, second_set_training, by = 1, all = TRUE)
    to.test <- to.test[rowSums(!is.na(to.test[,2:3]) * 1) > 1, ]
    first_v_second_train <- calc_statistics(to.test)
    
    to.test <- merge(code.to.use$computerSet, second_set_training, by = 1, all = TRUE)
    to.test <- to.test[rowSums(!is.na(to.test[,2:3]) * 1) > 1, ]
    second_v_classifier_train <- calc_statistics(to.test)
  }

  cloned_code <- code.to.use$clone(deep = TRUE)
  cloned_code$statistics <- NULL
  cloned_code$excerpts <- NULL
  cloned_code$holdoutExcerpts <- NULL
  cloned_code$touchableExcerpts <- NULL
  
  new_tests <- list(
    one_v_classifier = first_v_classifier,
    one_v_two = test_result(test_set = first_v_second, training_set = first_v_second_train),
    two_v_classifier = test_result(test_set = second_v_classifier, training_set = second_v_classifier_train),
    classifier = cloned_code
  )
  
  code.to.use$setValue("testedTestSet", TRUE)
  code.to.use$statistics[[length(code.to.use$statistics) + 1]] <- new_tests
  code.to.use
}

#' @title Calculate statistics
#' @description Run tests (kappa, rho) on the given Code
#'
#' @param code Code object to test
#' @param kappaThreshold Threshold used for calculating rhoR::rho
#' @param baserateInflation inflation rate to use when sampling handsets
#' @param type vector indicating which stats should be calculated
#'
#' @return Code object with updated statistics property
#' @export
old_test <- function(code, kappaThreshold = 0.65, baserateInflation = 0.2, type = c("training", "test")) {
  if(is.null(code) || !is(object=code, class=c("Code")))
    stop("Supplied `code` must be an instance of `Code`")

  code.to.use = code$clone(deep=T);
  code.to.use$baserateInflation = baserateInflation;
  
  #####
  # Helper method for returning set
  #####
    getSet <- function(wh = "test", compareTo = "computer") {
      to.test.set = code.to.use[[paste0(wh,"Set")]];
      # compare.to.set = code.to.use[[paste0(compareTo,"Set")]];
      if("computer" %in% compareTo) {
        compare.to.set = code.to.use$computerSet
        matrix(c(compare.to.set[to.test.set[,1]], to.test.set[,2]), ncol=2)
      } else {
        compare.to.set = do.call(rbind, lapply(paste0(compareTo,"Set"), function(x) code.to.use[[x]]))
        matrix(c(compare.to.set[(compare.to.set[,1] %in% to.test.set[,1]),2], to.test.set[,2]), ncol = 2)
      }
    }
  #####
  
  statResults = list();
  if("training" %in% type) {
    to.train = merge.sets(code.to.use, "trainingSet")[,-c(1)]
    trainKappa = list(kappa=NA)
    trainN = nrow(to.train)
    if(trainN>0) {
      trainKappa$kappa = rhoR::kappaSet(to.train)
    }
    trainKappa$N = trainN
    trainKappa$excerpts = c(NA)
    
    if(nrow(code$trainingSet) > 0) 
      trainKappa$excerpts = code$trainingSet[,1];
    
    results = Test$new(trainKappa,
      baserate = code.to.use$baserate,
      inflation = code.to.use$baserateInflation,
      thresholds = list(
        kappa = kappaThreshold
      )
    )
    # code.to.use$statistics$trainingSet[[length(code.to.use$statistics$trainingSet)+1]] = results
    code.to.use$statistics$trainingSet = c(results, code.to.use$statistics$trainingSet)
  }
  if("test" %in% type) {
    # to.test = getSet("test");
    to.test = merge.sets(code.to.use, "testSet")[,-c(1)]
    testRho = list(rho=NA, kappa=NA)
    testN = nrow(to.test)
    if(testN>0) {
      testRho = tryCatch(rhoR::rhoSet(to.test, ScSKappaThreshold = kappaThreshold, testSetBaserateInflation = baserateInflation), error = function(x) {
        list(rho = NA, kappa = rhoR::kappaSet(to.test))
      });
    }
    testRho$N = testN
    testRho$excerpts = c(NA)
    
    if(nrow(code$testSet) > 0)
      testRho$excerpts = code$testSet[,1];
    
    results = Test$new(testRho,
      baserate = code.to.use$baserate,
      inflation = code.to.use$baserateInflation,
      thresholds = list(
        kappa = kappaThreshold
      )
    )
    # code.to.use$statistics$testSet[[length(code.to.use$statistics$testSet)+1]] = results
    code.to.use$statistics$testSet = c(results, code.to.use$statistics$testSet)
    code.to.use$setValue("testedTestSet", T)
  }
  if("second" %in% type) {
    # to.second = getSet("secondRater", c("training", "test"))
    to.second = merge.sets(code, "secondRaterSet", c("trainingSet", "testSet"))
    
    secondStats = list(N = nrow(to.second))
    if(secondStats$N>0) {
      secondStats$kappa = rhoR::kappaSet(to.second[,2:3])
      secondStats$excerpts =  code.to.use$secondRaterSet[,1]
    }
    
    results = Test$new(secondStats,
      baserate = code.to.use$baserate,
      inflation = code.to.use$baserateInflation,
      thresholds = list(
        kappa = kappaThreshold
      )
    )
    # code.to.use$statistics$secondRaterSet[[length(code.to.use$statistics$secondRaterSet)+1]] = results;
    code.to.use$statistics$secondRaterSet = c(results, code.to.use$statistics$secondRaterSet);
  }

  # testResult = Test$new(list( 
  #   results = statResults, 
    # baserate = code$baserate,
    # baserateInflation = code.to.use$baserateInflation,
    # thresholds = list(
    #   kappa = kappaThreshold
    # )
  # ))
  
  # warning("This should somehow be returning a clone of the `code` provided")
  # code.to.use$statistics[[length(code.to.use$statistics)+1]] = testResult;
  
  code.to.use
}

#####
# Test Class
#####
  Test = R6::R6Class("Test",
    public = list(
      baserate = NULL,
      baserateInflation = NULL,
      # testSet = list(),
      # trainingSet = list(),
      # secondRaterSet = list(),
      thresholds = list(),
      set = list(),
      code = NULL,
      rho = NULL,
      kappa = NULL,
      N = NULL,
      excerpts = NULL,
      
      ###
      # Main class constructor
      ###
      initialize = function(stats,baserate,inflation,thresholds,code = NULL) {
        # if(!all(type %in% c("training", "test", "second"))) {
        #   warning("Type must be one or more of: c('training', 'test', 'second').")
        # } else {
          self$thresholds = thresholds; #stats$thresholds
          self$baserate = baserate;
          self$baserateInflation = inflation;
          
          lapply(names(stats), function(n) { self[[n]] = stats[[n]] })
          
          # self$trainingSet = stats$results$trainingSet
          # self$testSet = stats$results$testSet       
          # self$secondRaterSet = stats$results$secondRaterSet
          
          if(!is.null(code)) {
            self$code = code$clone(deep=T);
          }
        # }
      },
      print = function() {
        print(sapply(ls(self)[sapply(ls(self), function(x) !is.function(self[[x]]))], function(x) self[[x]]))
      }
    ),
    private = list(
    )
  )
#####

test_result <- function(test_set = NULL, training_set = NULL) {
  new_result <- list(
    test_set = test_set,
    training_set = training_set
  )
  class(new_result) <- c("ncoder.test_result", class(new_result))
  new_result
}

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
test <- function(code, kappaThreshold = 0.65, baserateInflation = 0.2, type = c("training", "test")) {
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
    to.train = getSet("training");
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
    code.to.use$statistics$trainingSet[[length(code.to.use$statistics$trainingSet)+1]] = results
  }
  if("test" %in% type) {
    to.test = getSet("test");
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
    code.to.use$statistics$testSet[[length(code.to.use$statistics$testSet)+1]] = results
    code.to.use$setValue("testedTestSet", T)
  }
  if("second" %in% type) {
    to.second = getSet("secondRater", c("training", "test"))
    
    secondStats = list(N = nrow(to.second))
    if(secondStats$N>0) {
      secondStats$kappa = rhoR::kappaSet(to.second)
      secondStats$excerpts =  code.to.use$secondRaterSet[,1]
    }
    
    results = Test$new(secondStats,
      baserate = code.to.use$baserate,
      inflation = code.to.use$baserateInflation,
      thresholds = list(
        kappa = kappaThreshold
      )
    )
    code.to.use$statistics$secondRaterSet[[length(code.to.use$statistics$secondRaterSet)+1]] = results
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

#' @title Calculate statistics
#' @description Run tests (kappa, rho) on the given Code
#'
#' @param code Code object to test
#' @param kappaThreshold Threshold used for calculating rhoR::rho
#'
#' @return Code object with updated statistics property
#' @export
test <- function(code, kappaThreshold=0.65) {
  if(is.null(code) || !is(object=code, class=c("Code")))
    stop("Supplied `code` must be an instance of `Code`")
  code.to.use = code$clone(deep=T);
  
  getSet <- function(wh = "test") {
    to.test.set = code.to.use[[paste0(wh,"Set")]];
    matrix(c(code.to.use$computerSet[to.test.set[,1]], to.test.set[,2]), ncol=2)
  }
  to.test = getSet("test")
  to.train = getSet("training")
  
  testRho = list(rho=NA, kappa=NA)
  trainKappa = list(kappa=NA)
  
  testN = nrow(to.test)
  trainN = nrow(to.train)
  if(testN>0) {
    testRho = tryCatch(rhoR::rhoSet(to.test, ScSKappaThreshold = kappaThreshold), error = function(x) {
     list(rho = NA, kappa = rhoR::kappaSet(to.test))
    });
  }
  if(trainN>0) {
    trainKappa$kappa = rhoR::kappaSet(to.train)
  }
  
  testRho$N = testN
  trainKappa$N = trainN
  
  testResult = Test$new(list( 
    results = list(
      testSet = testRho,
      trainingSet = trainKappa
    ), 
    baserate = code$baserate,
    baserateInflation = code$baserateInflation,
    thresholds = list(
      kappa = kappaThreshold
    )
  ))
  
  # warning("This should somehow be returning a clone of the `code` provided")
  code.to.use$statistics = testResult;
  
  code.to.use
}

Test = R6::R6Class("Test",
  public = list(
    baserate = NULL,
    baserateInflation = NULL,
    testSet = list(),
    trainingSet = list(),
    thresholds = list(),
    code = NULL,
    
    ###
    # Main class constructor
    ###
    initialize = function(stats, code = NULL) {
      self$testSet = stats$results$testSet       
      self$trainingSet = stats$results$trainingSet
      self$thresholds = stats$thresholds
      self$baserate = stats$baserate
      self$baserateInflation = stats$baserateInflation
      if(!is.null(code)) {
        self$code = code$clone(deep=T);
      }
    },
    print = function() {
      print(sapply(ls(self)[sapply(ls(self), function(x) !is.function(self[[x]]))], function(x) self[[x]]))
    }
  ),
  private = list(
  )
)

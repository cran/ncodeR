code.data = function(test, rater, amount = 10){
  if(class(test) != c("Test", "R6")){
    stop("test must be a \'Test\' Object");
  }
  
  test$codeData(rater = rater, amount = amount);
  
}
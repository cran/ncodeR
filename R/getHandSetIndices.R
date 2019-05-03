#' @title Handset indices
#' @description Handset indices
#' @param codeToUse [TBD]
#' @param handSetLength [TBD]
#' @param handSetBaserate [TBD]
#' @param unseen [TBD]
#' @export
getHandSetIndices = function(codeToUse, handSetLength = 20, handSetBaserate = 0.2, unseen = F) {
  set = codeToUse$computerSet
  set[c(codeToUse$trainingSet[,1], codeToUse$testSet[,1])] = NA
  positives = ceiling(handSetLength * handSetBaserate);
  posInd = which(set > 0 & !is.na(set));
  
  if(positives > length(posInd)){stop("Not enough positives in first rater to inflate to this level")}
  
  this.set = NULL
  if (positives > 0) {
    positiveIndices = posInd[sample.int(length(posInd),size=positives,replace=FALSE)];
    others = set[!(1:length(set) %in% positiveIndices) & !is.na(set)];
    otherIndices = sample.int(length(others),size=(handSetLength - positives),replace=FALSE);
    this.set = c(positiveIndices, otherIndices);
  } else if (positives == 0){
    theseIndices = sample((1:length(!is.na(set)))[!is.na(set)], size=handSetLength,replace=FALSE);
    this.set = theseIndices;
  }
  
  if(unseen == T) {
    print("Unseen excerpts is not yet implemented.")
  }
  
  return(sample(this.set));
}

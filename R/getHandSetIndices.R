#' @title Handset indices
#' @description Handset indices
#' @param set [TBD]
#' @param handSetLength [TBD]
#' @param handSetBaserate [TBD]
#' @export
getHandSetIndices = function(set, handSetLength = 20, handSetBaserate = 0.2) {
  positives = ceiling(handSetLength * handSetBaserate);
  posInd = which(set == 1);
  
  if(positives > length(posInd)){stop("Not enough positives in first rater to inflate to this level")}
  
  this.set = NULL
  if (positives > 0) {
    positiveIndices = posInd[sample.int(length(posInd),size=positives,replace=FALSE)];
    others = set[!(1:length(set) %in% positiveIndices)];
    otherIndices = sample.int(length(others),size=(handSetLength - positives),replace=FALSE);
    this.set = c(positiveIndices, otherIndices);
  } else if (positives == 0){
    theseIndices = sample.int(length(set),size=handSetLength,replace=FALSE);
    this.set = theseIndices;
  }
  
  return(sample(this.set));
}

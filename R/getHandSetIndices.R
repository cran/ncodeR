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


#' Get indices to code
#'
#' @param code Code object
#' @param handSetLength Number of excerpts to put into the test set
#' @param handSetBaserate Minimum number of positives that should be in the test set
#' @param unseen [TBD]
#' @param this.set [TBD]
#'
#' @return Code object with an updated test set and computer set
#' @export
getHandSetIndices2 = function(
  code, 
  handSetLength = 20, 
  handSetBaserate = .2, 
  unseen = F,
  this.set = NULL
) {
  codeToUse = code$clone(deep = T)
  positives = ceiling(handSetLength * handSetBaserate); # minimum number of positive indices needed
  maxNonPositives = handSetLength - positives; # maximum number of non-positive indices allowed
  
  # create data frame same length as excerpts -- used for keeping track of indices sampled
  if(is.null(this.set)) {
    indices = c(1:length(codeToUse$holdoutExcerpts))
    
    # set touchedIndices, testSet, and trainingSet to NA - ensure they won't be sampled again
    indices[codeToUse$touchedIndices] = NA
    indices[codeToUse$testSet[,1]] = NA
    indices[codeToUse$trainingSet[,1]] = NA
  
    # get miniumum number of positive indices necessary
    while(positives > 0){ 
      if(sum(is.na(indices)) == length(codeToUse$holdoutExcerpts) ){ # not enough positives to fill baserate
        stop("Not enough positives in first rater to inflate to this level")
      }
      
      randIndice = sample(indices[!is.na(indices)],1) # get a random indice
      indices[randIndice] = NA # set this indice to be NA, won't be sampled again
      
      # autocode excerpt at this single indice
      autocoded = codeToUse$process(codeToUse$excerpts[randIndice])
      codeToUse$touchedIndices = c(codeToUse$touchedIndices, randIndice)
      if(autocoded == 1){
        positives = positives - 1;
        this.set = c(this.set, randIndice) # only adding positive indices to handset
        # codeToUse$computerSet = rbind(codeToUse$computerSet, data.frame("ID" = randIndice, "X1" = autocoded))
        codeToUse$computerSet = rbind(codeToUse$computerSet, c(randIndice, autocoded))
      }
      else{ # deal with nonPositive excerpt case
        if(maxNonPositives != 0){ # room in handSet to add nonPositive -> add it
          this.set = c(this.set, randIndice)
          maxNonPositives = maxNonPositives - 1;
          # codeToUse$computerSet = rbind(codeToUse$computerSet, data.frame("ID" = randIndice, "X1" = autocoded))
          codeToUse$computerSet = rbind(codeToUse$computerSet, c(randIndice, autocoded))
        }
        else{ # if max number of nonPositve indices is reached, save indice, but don't add to set
          
        }
      }
    }
    
    # fill up the rest of handset with random indices, positive or not
    if(maxNonPositives != 0){
      for(i in 1:maxNonPositives){
        randIndice = sample(indices[!is.na(indices)],1)
        autocoded = codeToUse$process(codeToUse$excerpts[randIndice])
        indices[randIndice] = NA
        this.set = c(this.set, randIndice)
        codeToUse$computerSet = rbind(codeToUse$computerSet, c(randIndice, autocoded))
      }
    }
  
    if(unseen == T) {
      warning("Unseen excerpts is not yet implemented.")
    }
  
    # randomize output order of indices
    # codeToUse$testSet = rbind(
    #   codeToUse$testSet,
    #   #data.frame(ID=this.set[sample.int(n = handSetLength)], X1=NA)
    #   data.frame(ID=codeToUse$computerSet[,1][sample.int(n = handSetLength)], X1=NA)
    # )
    codeToUse$testSet = rbind(
      codeToUse$testSet,
      matrix(
        c(this.set[sample.int(n = handSetLength)], rep(NA, handSetLength)),
        ncol = 2, nrow = handSetLength,
        byrow = F, dimnames = list(NULL, colnames(codeToUse$testSet))
      )
    )
  }
  else {
    autocoded = codeToUse$process(codeToUse$excerpts[this.set])
    codeToUse$computerSet = rbind(codeToUse$computerSet, data.frame("ID" = this.set, "X1" = autocoded))
  
    codeToUse$touchedIndices <- c(codeToUse$touchedIndices, this.set)
    codeToUse$testSet = rbind(
      codeToUse$testSet,
      data.frame(ID=this.set, X1=NA)
    )
  }
  
  return(codeToUse); 
}
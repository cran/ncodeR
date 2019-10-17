#' Find rows that differ within a data.frame or two vectors
#' @title Find Differences
#' @description Find rows that differ within a data.frame or two vectors
#'
#' @param code Code object to search for differences
#' @param wh Set to use as the base comparison
#' @param to Set to compare wh to 
#'
#' @return logical vector representing indices that are coded differently
#' @export 
#' @return vector of indices representing differences
differences <- function(code = NULL, wh = "trainingSet", to = "computerSet") {
  comparing = merge.sets(code, wh, to);
  # comparing[comparing[,2] != comparing[,3], 1]
  # comparing[rowSums(comparing[,-c(1)]) == 1, 1]
  comparing[comparing[,2] != comparing[,3], 1]
}

merge.sets <- function(code, wh = "trainingSet", to = "computerSet") {
  ids = code[[wh]][,1]
  wh.indices = !ids %in% code$ignoredSet & !is.na(code[[wh]][,2])
  ids.to.use = ids[wh.indices]
  
  if(length(ids.to.use) > 0) {
    # if(length(to) > 1) {
      compare.to = combine.sets(code, to);
      compare.to = compare.to[compare.to[,1] %in% ids.to.use,];
    # } 
    # else {
    #   compare.to = data.frame(ID=ids.to.use, X1=code[[to]][ids.to.use])
    # }
    
    # merge(unique(code[[wh]]), unique(compare.to), sort = F, by = "ID")
    merge(unique(code[[wh]]), unique(compare.to), sort = F, by = 1)
  } else {
    # data.frame(ID=NULL, X1=NULL)
    structure(list(ID = NULL, X1 = NULL, X2 = NULL), class = "data.frame")
  } 
}

combine.sets <- function(code, wh) {
  compare.sets = lapply(wh, function(tt) code[[tt]])
  do.call("rbind", compare.sets);
}
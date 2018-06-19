#' Find rows that differ within a data.frame or two vectors
#' @title Find Differences
#' @description Find rows that differ within a data.frame or two vectors
#'
#' @param code Code object to search for differences
#'
#' @return logical vector representing indices that are coded differently
#' @export 
differences <- function(code = NULL) { #, data = NULL, col1 = NULL, col2 = NULL, cols = NULL) {
  # if(!is.null(code)) {
  ids = code$trainingSet[,"ID"]
  ids.to.use = !ids %in% code$ignoredSet & !is.na(ids)
  ids[!ids.to.use] = NA
  ids.to.use[!ids.to.use] = NA
  #   data = matrix(
  #     c(
  #       code$computerSet[ids[ids.to.use]],
  #       code$trainingSet[ids.to.use,2]
  #     ), 
  #     ncol=2
  #   );
  # }
  # if(!is.null(data)) {
  #   if (!is.null(cols) && length(cols)==2) {
  #     data = data[,cols];
  #   }
  #   all.numeric.cols = all(apply(data, 2, is.numeric))
  #   
  #   if(ncol(data) != 2 || !all.numeric.cols) {
  #     stop("Data must be two columns, both of which must be numeric.")
  #   }
  # } else if (!is.null(col1) && !is.null(col2)) {
  #   data = data.frame(col1=col1, col2=col2)
  # }
  
  ids[which(code$computerSet[ids] != code$trainingSet[ids.to.use,2])]
}
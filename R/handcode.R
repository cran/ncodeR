#' #' Start validation of a code
#' #' @description Validate a code by asking the user for input
#' #' @import cli
#' #' @export
#' validate = function(code, numExcerpts = 10, baserate = 0.2, exclude = c(), auto.update = T) {
#'   if(!is(code, "Code")){  
#'     stop("code must be an instance of class 'Code' or one of its subclasses");
#'   }
#'   if(numExcerpts < 1 || class(numExcerpts) != "numeric") {
#'     stop("numExcerpts must be a positive integer");
#'   }
#'   if(baserate < 0 || baserate > 1) {
#'     stop("baserate must be a positive real number between one and zero (inclusive)");
#'   }
#'   
#'   uncodedInSet = !(1:length(code$computerSet) %in% which(code$resultsSet$ID > 0))
#'   indices = rhoR:::getHandSetIndices(code$computerSet[uncodedInSet], handSetLength = numExcerpts, handSetBaserate = baserate)
#'   
#'   newTest = matrix(NA, ncol=3)
#'   selfCodes = handcode(code$codeSet$excerpts[indices])
#'   newTest = matrix(c(indices,selfCodes,code$computerSet[indices,c(code$name)]), dimnames=list(NULL,c("ID","self","computer")), ncol=3)
#'   
#'   if(auto.update == T) {
#'     code$resultsSet = rbind(code$resultsSet,matrix(c(indices, selfCodes, rep(NA,length(selfCodes))), ncol=3, dimnames=list(NULL,dimnames(code$resultsSet)[[2]])))
#'     return(code$testSet());
#'   } else {
#'     return(newTest);
#'   }
#' }

#' @title Handcode excerpts
#' @description Handcode a set of excerpts using a vector of expressions
#' 
#' @param code Code object to handcode
#' @param excerpts Excerpts to code (optional)
#' @param expressions Expressions to code with (options)
#' @param n Number of excerpts to handcode
#' @param baserate Value between 0 and 1, inflates the baserate chosen excerpts to code, ensuring the number of positive at least equal to n * baserate 
#'
#' @import cli
#' @export
handcode = function(
  code = NULL, 
  excerpts = NULL, 
  expressions = NULL, 
  n = 10, 
  baserate = 0.2
) {
  code.to.use = NULL;
  if(!is.null(code)) {
    code.to.use = code$clone(deep = T);
  } else {
    code.to.use = create.code();
  }
  if(!is.null(excerpts)) {
    code.to.use$excerpts = excerpts;
  }
  if(!is.null(expressions)) {
    code.to.use$expressions = expressions;
  }
 
  code.to.use$baserateInflation = baserate; 
  len = 0
  if(!is.null(excerpts) && length(excerpts) > 0) {
    len = length(excerpts);
  } else if (!is.null(code.to.use) && length(code.to.use$excerpts) > 0) {
    len = length(code.to.use$excerpts);
    excerpts = code.to.use$excerpts;
  } 
  
  if(len < 1) {
    stop("No excerpts found. Either add excerpts to the code$codeSet or use the excerpts parameter")
  }
  
  code.to.use = autocode(code = code.to.use, expressions = code.to.use$expressions, excerpts = code.to.use$excerpts, simplify=F)
  
  # uncodedInSet = !(1:length(code$computerSet) %in% which(code$resultsSet$ID > 0))
  # indices = rhoR:::getHandSetIndices(code$computerSet[uncodedInSet], handSetLength = numExcerpts, handSetBaserate = baserate)
  indices = getHandSetIndices(code.to.use$computerSet, handSetLength = n, handSetBaserate = baserate)
  
  # selfCodes = c()
  # for(i in 1:length(excerpts)) {
  #   index = i;
  #   print(boxx(c(strwrap(as.character(excerpts[index]))), width=50))
  #   uin = readline("Enter (y/n): ");
  #   selfCodes = c(selfCodes, grepl(x=tolower(uin),pattern="^y",perl=T)*1)
  # }
  len = length(indices)
  coding = T;
  recoding = F;
  numberToReview = -1;
  while(coding) {
    # useIndices = indices;
    # if(numberToReview != -1) {
    #   useIndices = useIndices[numberToReview]
    # }
    
    selfCodes = matrix(c(indices, sapply(indices, function(index) {
      indexCount = which(indices == index)
      if(recoding && numberToReview != -1 && indexCount != numberToReview) {
        cur = selfCodes[selfCodes[,1] == index,2];
        return(cur)
      }

      print(cli::boxx(c(
        paste0("Excerpt #",indexCount),
        paste0("Definition: ", code$definition),
        "", 
        strwrap(as.character(excerpts[index]))
      ), width=50))
      
      if(recoding) {
        cur = selfCodes[selfCodes[,1] == index,2];
        cat("Current:", cur,"\n")
        uin = readline("Change? (y/n): ");
        if(grepl(x=tolower(uin),pattern="^y",perl=T)) {
          return((!as.logical(cur)) * 1)
        } else {
          cur
        }
      } else {
        uin = readline("Enter (y/n): ");
        grepl(x=tolower(uin),pattern="^y",perl=T)*1
      }
    })), nrow = len);
    
    keepOn = readline("Would you like to review? (all/#/none): ");
    if(is.numeric(type.convert(keepOn))) {
      numberToReview = type.convert(keepOn)
      coding = recoding = T
    } else {
      numberToReview = -1
      coding = recoding = grepl(x=tolower(keepOn),pattern="^a",perl=T)
    }
    # coding = recoding = grepl(x=tolower(keepOn),pattern="^y",perl=T)
  }
  
  code.to.use$testSet = rbind(code.to.use$testSet, selfCodes)
  
  # if(!is.null(expressions)) {
  #   autoCodes = code(expressions, excerpts)
  #   retMat = matrix(c(selfCodes, autoCodes), ncol= 2, dimnames=list(NULL,c("self","computer")))
  #   return(retMat);
  # } else {
  #   return(selfCodes);
  # }
  
  code.to.use
}
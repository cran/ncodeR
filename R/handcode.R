#' Handcode a set of excerpts using a vector of expressions
#' @title Handcode excerpts
#' @description Handcode a set of excerpts using a vector of expressions
#' 
#' @param code Code object to handcode
#' @param excerpts Excerpts to code (optional)
#' @param expressions Expressions to code with (options)
#' @param n Number of excerpts to handcode
#' @param baserate Value between 0 and 1, inflates the baserate chosen excerpts to code, ensuring the number of positive at least equal to n * baserate 
#' @param unseen Logical or number Indicating additional excerpts with unseen words should be added. If TRUE (default), two words added or by `number`
#' @param this.set [TBD]
#' @param results [TBD]
#' 
#' @import cli
#' @export
#' @return Code
handcode = function(
  code = NULL, 
  excerpts = NULL, 
  expressions = NULL, 
  n = ifelse(is.null(this.set), 10, length(this.set)), 
  baserate = 0.2,
  unseen = F,
  this.set = NULL,
  results = NULL
) {
  code.to.use = NULL;
  if(!is.null(code)) {
    code.to.use = code$clone(deep = T);
  }
  else {
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
  }
  else if (!is.null(code.to.use) && length(code.to.use$excerpts) > 0) {
    len = length(code.to.use$excerpts);
    excerpts = code.to.use$excerpts;
  } 
  
  if(len < 1) {
    stop("No excerpts found. Either add excerpts to the code$codeSet or use the excerpts parameter")
  }
  
  if(code.to.use$getValue("testedTestSet") == T) {
    if(is.null(results)) {
      cat("This TestSet has already been tested, continuing will move your current Test into Training.")
      uin = readline("Continue in creating a new TestSet? (Yes/no [default: no]): ");
      if(grepl(x=tolower(uin),pattern="^y",perl=T)) {
        code.to.use$clearTestSet()
      } else {
        return(code.to.use)
      }
    } else {
      code.to.use$clearTestSet()
    }
  }
  
  # # uncodedInSet = !(1:length(code$computerSet) %in% which(code$resultsSet$ID > 0))
  # # indices = rhoR:::getHandSetIndices(code$computerSet[uncodedInSet], handSetLength = numExcerpts, handSetBaserate = baserate)2
  
  #code.to.use = autocode(x = code.to.use, expressions = code.to.use$expressions, excerpts = code.to.use$excerpts, simplify=F)
  # code.to.use = getHandSetIndices2(code.to.use, handSetLength = n, handSetBaserate = baserate, unseen = unseen)
  
  code.to.use = getHandSetIndices2(
    code = code.to.use,
    handSetLength = n,
    handSetBaserate = baserate,
    unseen = unseen,
    this.set = this.set
  )
  indices = code.to.use$testSet[,1]
  
  ## New handset code
  # indices = getHandSetIndices2(code.to.use, handSetLength = n, handSetBaserate = baserate, unseen = unseen)
  # code.to.use$computerSet = code.to.use$process(code.to.use$excerpts[indices])
  
  # selfCodes = c()
  # for(i in 1:length(excerpts)) {
  #   index = i;
  #   print(boxx(c(strwrap(as.character(excerpts[index]))), width=50))
  #   uin = readline("Enter (y/n): ");
  #   selfCodes = c(selfCodes, grepl(x=tolower(uin),pattern="^y",perl=T)*1)
  # }
  len = length(indices)
  coding = TRUE;
  recoding = FALSE;
  numberToReview = -1;
  
  if(is.null(results)) {
    while(coding) {
      selfCodes = matrix(c(indices, sapply(indices, function(index) {
        indexCount = which(indices == index)
        if(recoding && numberToReview != -1 && indexCount != numberToReview) {
          cur = selfCodes[selfCodes[,1] == index,2];
          return(cur)
        }
  
        print(cli::boxx(c(
          paste0("ID: ", index),
          paste0("Excerpt #",indexCount),
          paste0("Definition: ", code$definition),
          "", 
          strwrap(as.character(sub(pattern = "\n", replacement = " ", perl = T, x = excerpts[index])))
        ), width=50))
        
        if(recoding) {
          cur = selfCodes[selfCodes[,1] == index,2];
          cat("Current:", cur,"\n")
          uin = readline("Change? (y/n): ");
          if(grepl(x=tolower(uin),pattern="^y",perl=T)) {
            return((!as.logical(cur)) * 1)
          }
          else {
            cur
          }
        }
        else {
          uin = readline("Enter (y/n): ");
          grepl(x=tolower(uin),pattern="^y",perl=T)*1
        }
      })), nrow = len);
      
      keepOn = readline("Would you like to review? (all/#/none): ");
      if(is.numeric(type.convert(keepOn))) {
        numberToReview = type.convert(keepOn)
        coding = recoding = T
      }
      else {
        numberToReview = -1
        coding = recoding = grepl(x=tolower(keepOn),pattern="^a",perl=T)
      }
    }
  }
  else {
    selfCodes = matrix(c(
      this.set,
      rep(results, length(this.set) / length(results))
    ), ncol = 2)
  }
  
  # code.to.use$testSet = rbind(code.to.use$testSet, selfCodes)
  code.to.use$testSet[code.to.use$testSet[,1] == selfCodes[,1] ,2] = selfCodes[, 2]
  
  # if(!is.null(expressions)) {
  #   autoCodes = code(expressions, excerpts)
  #   retMat = matrix(c(selfCodes, autoCodes), ncol= 2, dimnames=list(NULL,c("self","computer")))
  #   return(retMat);
  # } else {
  #   return(selfCodes);
  # }
  
  code.to.use
}
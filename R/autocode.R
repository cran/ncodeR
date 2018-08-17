#' Match a list of expressions against some set of excerpts
#'
#' @description Autocodes all codes provided, either directly with code or as part of a provided codeset
#' 
#' @param code Code to autocode
#' @param expressions Expressions to use for coding (optional)
#' @param excerpts Excerpts to code
#' @param codeSet Codeset to code, required if code == NULL
#' @param simplify If TRUE, returns a data.frame, else returns a Code or CodeSet object 
#' @param mode Either all, training, or test representing the set of excerpts that should be recoded in the computerSet
#'
#' @return data.frame of is simplify = T (default), otherwise the Code or CodeSet object with updated computerSets
#' 
#' @export
autocode <- function(code = NULL, expressions = NULL, excerpts = NULL, codeSet = NULL, simplify = T, mode = "all") {
  modes = c("all", "training", "test")
  to.code = NULL;
  code.to.use = NULL;
  codeSet.to.use = NULL;
  mode = match.arg(mode, modes);
  if(!is.null(codeSet)) {
    codeSet.to.use = codeSet$clone(deep = T);
    to.code = sapply(codeSet.to.use$codes, function(cc) { cc$clone(deep = T); cc$codeSet = codeSet.to.use; cc });
  } else {
    codeSet.to.use = CodeSet$new(
      title = "NewCodeSet", 
      description = "New CodeSet for Codes", 
      excerpts = to.code[[1]]$excerpts
    )
    if (!is.null(code)) {
      to.code = c(code$clone(deep = T))
    } else {
      to.code = c(create.code(excerpts = excerpts))
    }
    codeSet.to.use$codes = to.code
  } 

  if(!is.null(excerpts)) {
    codeSet.to.use$excerpts = excerpts;
  }
  for(coding in 1:length(to.code)) {
    expressions.to.use = c()
    code.to.use = to.code[[coding]]
    
    if(!is.null(expressions)) {
      code.to.use$expressions = expressions;
    }
    
    expressions.to.use = sapply(code.to.use$expressions, function(x) {
      ifelse(grepl(pattern="^[[:alnum:]]*$", x=x, perl=T), paste0("\\b",x), x)
    })
    
    len = 0
    if (is.null(excerpts)) {
      if(!is.null(code.to.use) && length(code.to.use$excerpts) > 0) {
        if (mode == "training") {
          excerpts = code.to.use$excerpts[code.to.use$trainingSet[,1]]
        } else {
          excerpts = codeSet.to.use$excerpts;
        }
      }
    } 
    len = length(excerpts);
    
    if(len < 1) {
      stop("No excerpts found. Either add excerpts to the code or use the excerpts parameter")
    }
    
    codedResults = code.to.use$computerSet = (rowSums(data.frame(lapply(expressions.to.use, grepl, excerpts, perl = T, ignore.case = T)) * 1) >= 1) * 1;
    if(mode == "training") {
      code.to.use$computerSet[code.to.use$trainingSet[,1]] = codedResults
    } else {
      code.to.use$computerSet = codedResults
    }
    code.to.use$baserate = rhoR::baserateSet(matrix(c(code.to.use$computerSet,code.to.use$computerSet),ncol=2))$firstBaserate
  }
  
  if(!is.null(code)) {
    if(simplify == T) {
      as.data.frame.Code(to.code[[1]])
    } else {
      to.code[[1]]
    }
  } else if(!is.null(codeSet)) {
    if(simplify == T) {
      browser()
      as.data.frame.CodeSet(codeSet.to.use)
    } else {
      codeSet.to.use$codes = to.code;
      codeSet.to.use
    }
  } else {
    if(simplify == T) {
      as.data.frame.Code(to.code[[1]])
    } else {
      to.code[[1]]
    }
  }
}
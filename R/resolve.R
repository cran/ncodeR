#' @title Resolve differences
#' @description Resolve differing results
#'
#' @param code Code to resolve coding differences 
#' @param trainingSet Optionally provide a trainingSet, default: code$trainingSet
#' @param computerSet Optionally provide a computerSet, default: code$computerSet
#' @param expressions Optionally provide a set of expressions, default: code$expressions
#' @param excerpts Optionally provide a set of excerpts, default: code$excerpts
#' @param ignored Optionally proivde a set of excerpts to ignore during the resolve cycle loop
#'
#' @export 
resolve <- function(code = NULL, trainingSet = NULL, computerSet = NULL, expressions = NULL, excerpts = NULL, ignored = NULL) {
  code.to.use = NULL;
  if(!is.null(code)) {
    code.to.use = code$clone(deep = T);
  } else {
    code.to.use = create.code();
  }
  if(!is.null(excerpts)) {
    code.to.use$excerpts = excerpts;
  } else {
    excerpts = code.to.use$excerpts;
  }
  if(!is.null(expressions)) {
    code.to.use$expressions = expressions;
  }
  if(!is.null(ignored)) {
    code.to.use$ignoredSet = ignored;
  } else if(is.null(code.to.use$ignoredSet)) {
    ignored = c();
    code.to.use$ignoredSet = ignored;
  }
  
  
  resolveFirst <- function() {
    cat("The TestSet hasn't been tested. If you continue, those results will be added to the TrainingSet.\n\n")
    cat("Would you like to: \n")
    cat("\t1.) Run a test first\n")
    cat("\t2.) Continue to resolve\n")
    cat("\t3.) Cancel [default]\n")
    
    uin = readline("Enter 1, 2, or 3: ")
    if(uin == "1") {
      code.to.use <<- test(code = code.to.use)
      teststats = code.to.use$statistics$testSet
      print(cli::boxx(
        c("Test Set Statistics",
          cli::rule(width = 30),
          paste0("kappa: ", sprintf("%.2f",round(teststats[[length(teststats)]]$kappa,2))),
          # paste0("\U03C1: ",   sprintf("%.2f",round(stats$results$testSet$rho,2)))
          paste0("rho: ",   sprintf("%.2f",round(teststats[[length(teststats)]]$rho,2)))
        )
      ));
      
      cat("Would you like to: \n")
      cat("1.) Proceed to resolve\n");
      cat("2.) Quit\n");
      return(readline("Enter 1 or 2: ") == "1")
    } else if (uin == "2") {
      return(T)
    } else {
      return(F)
    }
  }
  whichOption <- function() {
    cat("1. Change my code\n2. Add word to expression list\n3. Remove word form expression list\n4. Ignore difference\n")
    uin = readline("Enter 1, 2, 3, or 4: ");
    if(! (uin %in% c("1","2","3","4"))) {
      uin = whichOption()
    }
    uin
  }
  removeExpression <- function(codeClone, index) {
    matchedOn = codeClone$expressions[which(sapply(codeClone$expressions, function(expr) {
      expr = ifelse(grepl(pattern="^[[:alnum:]]*$", x=expr, perl=T), paste0("\\b",expr), expr)
      grepl(pattern = expr, x = as.character(excerpts[index]), perl=T, ignore.case=T)
    }))]
    for(i in 1:length(matchedOn)) { 
      cat(paste0(i, ")"), matchedOn[i], "\n") 
    }
    uin = readline("Number to remove: ");
    if(uin %in% as.character(1:length(matchedOn))) {
      codeClone$expressions = codeClone$expressions[!(codeClone$expressions %in% matchedOn[as.integer(uin)])]
    }
    codeClone
  }
  addExpression <- function(codeClone) {
    newWord = readline("Word(s) to add to the code (separate by |AND|): ");
    
    if(!is.null(newWord) && newWord != "") {
      codeClone$expressions = c(codeClone$expressions, strsplit(x = newWord, split = "\\s*?\\|AND\\|\\s*", perl=T)[[1]])
    }
    
    codeClone
  }
  loopThem <- function(code.to.clone) {
    codeClone = code.to.clone$clone(deep = T);
    diffs = differences(codeClone);
    diff.rows = codeClone$trainingSet[,"ID",drop=F] %in% diffs
    diffData = codeClone$trainingSet[diff.rows,,drop=F]
    diffKappa = NA;
    for(i in 1:nrow(diffData)) {
      index = diffData[,"ID"][[i]];
      if(diffData[i,2] != codeClone$computerSet[index]) {
        matchedOn = codeClone$expressions[which(sapply(codeClone$expressions, function(expr) {
          expr = ifelse(grepl(pattern="^[[:alnum:]]*$", x=expr, perl=T), paste0("\\b",expr), expr)
          grepl(pattern = expr, x = as.character(excerpts[index]), perl=T, ignore.case=T)
        }))]
        diffKappa = codeClone$kappa()
        print(cli::boxx(
          c(
            paste0("Difference #",i),
            paste0("Training Kappa: ", sprintf("%.2f",round(diffKappa,2))),
            cli::rule(width = 50),
            strwrap(as.character(excerpts[index])), "", 
            paste0("Self: ", diffData[i,2]),
            paste0("Computer: ", codeClone$computerSet[index]),
            ifelse(length(matchedOn) > 0, paste0("Matched on: ", paste(matchedOn, collapse=",")), "")
          ), 
          width=50
        ));
        
        uin = whichOption()
        if(uin == "1") {
          codeClone$trainingSet[codeClone$trainingSet[,"ID"]==index,2] = (!as.logical(diffData[i,2])) * 1
        } else if(uin == "2" || uin == "3") {
          accept = F;
          while(!accept) {
            if(uin == 2){
              codeClone.new = addExpression(codeClone)
            }else{
              codeClone.new = removeExpression(codeClone, index)
            }
            codeClone.new = autocode(x = codeClone.new, simplify = F, mode = "training")
            newKappa = codeClone.new$kappa()
            percChange = newKappa - diffKappa
            cat(paste0(
              "Training Kappa with changes: ",
              sprintf("%.2f", round(newKappa, 2)), " (", ifelse(percChange>0,"+",ifelse(percChange<0,"-","")),  float(abs(percChange), 2), ")\n"
            ))
            accept = grepl(x=tolower(readline("Accept the change (y/n): ")),pattern="^y",perl=T)
            if(accept) {
              codeClone = codeClone.new$clone(deep = T)
            }
          }
        } else {
          cat(paste0("Ignoring difference #", i), "\n")
          ignored = c(ignored, index);
        }
      }
    }
    
    codeClone$ignoredSet = ignored
    codeClone
  }
  
  if(!is.null(trainingSet)) {
    code.to.use$trainingSet = trainingSet;
  } else {
    if(nrow(code.to.use$testSet) && code.to.use$getValue("testedTestSet") == F) {
      justResolve = resolveFirst()
      if(justResolve != T) {
        return(code.to.use)
      }
    }
    
    code.to.use$clearTestSet()
  }
  
  diffs = differences(code.to.use)
  continue = length(diffs) > 0;
  
  origKappa = NA;
  while(continue) {
    origKappa = code.to.use$kappa();
    code.to.use = loopThem(code.to.use);
    code.to.use = test(code.to.use)
    cat("New training kappa: ", float(code.to.use$kappa(which = "training")), "\n") #rhoR::kappa(trainingSet[,-c(1)]), "\n")
    diffs = differences(code.to.use)
    if(length(diffs) > 0) {
      cat("There are still", length(diffs), "difference(s).")
      keepOn = readline("Continue resolving? (y/n): ");
      continue = grepl(x=tolower(keepOn),pattern="^y",perl=T)
    } else {
      continue = F
    }
  }
  # code.to.use$trainingSet = trainingSet;
  
  cat("There are no more differences.\n")
  
  code.to.use = test(code.to.use)
  newKappa = code.to.use$kappa(which = "training")
  percChangeText = NULL
  if(!is.na(origKappa)) {
    percChange = newKappa - origKappa
    percChangeText = paste0(" (", ifelse(percChange>0,"+",ifelse(percChange<0,"-","")),  float(abs(percChange), 2), ") ")
  }
  
  
  print(cli::boxx(
    c("Training Set Statistics",
      cli::rule(width = 30),
      paste0("kappa: ", float(newKappa), percChangeText),
      paste0("N: ", nrow(code.to.use$trainingSet)),
      paste0("Ignored: ", length(code.to.use$ignoredSet))
    )
  ));
  
  code.to.use
}
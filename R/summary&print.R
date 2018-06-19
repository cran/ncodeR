bold <- function(txt){
  # paste0("\033[1m",txt,"\033[0m")
  txt
}
float <- function(n, p=2) {
  sprintf(paste0("%.",p,"f"), round(if(!is.null(n)) n else 0, p))
}

#' Obtain a summary of the CodeSet
#'
#' @param object CodeSet object
#' @param ... Additional parameters
#'
#' @examples
#' data(RS.data)
#' rs = RS.data
#' 
#' newcode = create.code(name = "Data", 
#'     expressions = c("number","data"), excerpts = rs$text)
#' code.set = code.set("Demo RS CodeSet", "CodeSet made for the demo", 
#'     excerpts = rs$text, codes = c(newcode))
#' summary(code.set)
#'
#' @return list containing description and Code summaries
#' @export
summary.CodeSet = function(object, ...) {
  this.summary = list(
    object$title, object$description,
    lapply(object$codes, function(x) summary(x))
  ) 
  #(object$codes);
  
  class(this.summary) = "summary.CodeSet"
  this.summary
}

#' Print the summary of a CodeSet
#'
#' @param x Summary of a CodeSet
#' @param ... Additional parameters
#'
#' @examples
#' data(RS.data)
#' rs = RS.data
#' 
#' newcode = create.code(name = "Data", 
#'     expressions = c("number","data"), excerpts = rs$text)
#' code.set = code.set("Demo RS CodeSet", "CodeSet made for the demo", 
#'     excerpts = rs$text, codes = c(newcode))
#' summary(code.set)
#'
#' @return prints summary
#' @export
print.summary.CodeSet = function(x, ...){
  writeLines(c(
    paste0("Title: ", x[[1]]),
    paste0("Description: ", x[[2]]),
    paste0("Codes: ")
  ))
  printedCodes = lapply(x[[3]], function(y) {
    cat("\n")
    print(y)
  })
  
}

#' Obtain a summary of a Code's test results
#'
#' @param object Test object of Code
#' @param ... Additional parameters
#'
#' @examples
#' data(RS.data)
#' rs = RS.data
#' newcode = create.code(name = "Data", 
#'     expressions = c("number","data"), excerpts = rs$text)
#' #newcode = handcode(code = newcode, excerpts = rs$text, n = 4)
#' newcode = test(code = newcode, kappaThreshold = 0.65)
#' summary(newcode$statistics)
#' @return list of Test summary
#' @export
summary.Test = function(object, ...) {
  args = list(...)
  
  which.test = c("test","training");
  if(!is.null(which.test)) which.test = args$which.test;
  
  if(!all(which.test %in% c("test","training"))) 
    stop("which.test may only contain 'test' and/or 'training'")
  this.summary = c(
    object$testSet$kappa, 
    object$testSet$rho, 
    object$testSet$N, 
    object$trainingSet$kappa, 
    object$trainingSet$N
  )
  
  class(this.summary) = "summary.Test"
  this.summary
}

#' Print a Test summary
#'
#' @param x list from summary()
#' @param ... Additional parameters
#' 
#' @examples
#' data(RS.data)
#' rs = RS.data
#' newcode = create.code( name = "Data", 
#'     expressions = c("number","data"), excerpts = rs$text)
#' #newcode = handcode(code = newcode, excerpts = rs$text, n = 4)
#' newcode = test(code = newcode, kappaThreshold = 0.65)
#' summary(newcode$statistics)
#' @return prints summary
#' @export
print.summary.Test = function(x, ...) {
  args = list(...)
  width = 40;
  if(!is.null(args$width)) width = args$width
  writeLines(c(
    paste0(rep("-",width), collapse=""),
    paste0(bold("Test Set"),"\t\t| ",bold("Training Set")),
    paste0(rep("-",width), collapse=""),
    # "kappa\t| \U03C1\t| N\t| kappa\t| N",
    "kappa\t| rho\t| N\t| kappa\t| N",
    paste0(rep("-",width), collapse=""),
    paste0(float(x[1]),"\t| ",float(x[2]),"\t| ",x[3],"\t| ", float(x[4]),"\t| ",x[5]),
    paste0(rep("-",width), collapse=""),
    ""
  ))
}

#' Obtain summary of a Code object
#'
#' @param object Code to summarize
#' @param ... Additional parameters
#'
#' @examples
#' data(RS.data)
#' rs = RS.data
#' newcode = create.code(name = "Data", 
#'     expressions = c("number","data"), excerpts = rs$text)
#' summary(newcode)
#'
#' @return List of Code summary
#' @export
summary.Code = function( object, ... ) {
  statsSummary = summary(object$statistics)
  this.summary = list(
    object$name,
    object$codeSet$title,
    object$definition, 
    object$examples,
    statsSummary,
    object$expressions,
    object$baserate,
    object$baserateInflation
  )
  
  class(this.summary) = "summary.Code"
  this.summary
}

#' Print a Code summary
#'
#' @param x list from summary()
#' @param ... Additional parameters
#'
#' @examples
#' data(RS.data)
#' rs = RS.data
#' newcode = create.code(name = "Data", 
#'     expressions = c("number","data"), excerpts = rs$text)
#' summary(newcode)
#'
#' @return Prints code summary
#' @export
print.summary.Code = function(x, ...) {
  args = list(...)
  width = 40;
  if(!is.null(args$width)) width = args$width
  
  to.write = c(
    paste0("Name: ", x[[1]]),
    paste0("Baserate: ", float(x[[7]])),
    paste0("Baserate Inflation: ", float(x[[8]]))
  );
  if(!is.null(x[[3]]) && x[[3]] != "")
    to.write = c(to.write, paste0("Definition: ", x[[3]]))
  if(!is.null(x[[4]]))
    to.write = c(to.write, paste0("Examples: ", x[[4]]))
  if(!is.null(x[[6]]))
    to.write = c(to.write, paste0("Expressions: ", paste(x[[6]], collapse=", ")))
  
  to.write = c(to.write, "Statistics: ")
  
  writeLines(to.write)
  print(x[[5]])
}


###
# Prints off the contents of the code
###
# print.Code = function(self){
#   print(cli::boxx("Code Details", cli::rule(), paste0("Name: ", self$name), paste0("Conceptual Definition: ", self$conceptDef)));
# }
###
# Prints off the code's expression list
###
# print.RegexCode = function(self){
#   print(cli::boxx(
#     c("Code Details", 
#       cli::rule( width = 50), 
#       paste0("Name: ", self$name), 
#       paste0("Conceptual Definition: ", self$conceptDef),
#       "Expressions: ",
#       cli::boxx(paste("",self$expressions, sep=""), width = 30, border_style = "none", margin = 0, padding = c(0,1,0,2))), width = 70, border_style = "none", padding = 0
#   ));
# }

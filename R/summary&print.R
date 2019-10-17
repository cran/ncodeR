bold <- function(txt){
  # paste0("\033[1m",txt,"\033[0m")
  txt
}
float <- function(n, p=2) {
  if(is.character(n))
    n = type.convert(n)
  
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
#' @param object TestList object of Code
#' @param ... Additional parameters
#'
#' @examples
#' data(RS.data)
#' rs = RS.data
#' newcode = create.code(name = "Data", 
#'     expressions = c("number","data"), excerpts = rs$text)
#' newcode <- handcode(newcode, this.set = 10:15, results = 0)
#' newcode = test(code = newcode, kappa_threshold = 0.65)
#' summary(newcode$statistics)
#' @return list of Test summary
#' @export
summary.TestList = function(object, ...) {
  args = list(...)
  
  stats <- object[[length(object)]]$one_v_classifier
  
  summary = list(
    stats$test_set$kappa, stats$test_set$N, stats$test_set$rho,
    stats$training_set$kappa, stats$training_set$N
  ) 
  
  class(summary) = "summary.TestList"
  summary
}

#' Print a TestList summary
#'
#' @param x list from summary()
#' @param ... Additional parameters
#' 
#' @examples
#' data(RS.data)
#' rs = RS.data
#' newcode <- create.code("Data", expressions = c("number","data"), excerpts = rs$text)
#' newcode <- handcode(newcode, this.set = 10:15, results = 0)
#' newcode = test(code = newcode, kappa_threshold = 0.65)
#' summary(newcode$statistics)
#' 
#' @return prints summary
#' @export
print.summary.TestList = function(x, ...) {
  args = list(...)
  width = 40;
  
  if(!is.null(args$width)) width = args$width
  
  test_vals = paste0(float(x[[1]]),"\t| ",float(x[[3]]),"\t| ",x[[2]])
  
  training_vals = paste0(float(x[[4]]),"\t| ",x[[5]])
  
  writeLines(c(
    "\nTest Set",
    paste0(rep("-",width), collapse=""),
    "kappa\t| rho\t| N",
    paste0(rep("-",width), collapse=""),
    test_vals,
    paste0(rep("-",width), collapse=""),
    "\nTraining Set",
    paste0(rep("-",width), collapse=""),
    "kappa\t| N",
    paste0(rep("-",width), collapse=""),
    training_vals,
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
  statsSummary <- NULL
  if(length(object$statistics)) {
    statsSummary <- summary(object$statistics)
  }
  
  this.summary <- list(
    object$name,
    object$codeSet$title,
    object$definition, 
    object$examples,
    statsSummary,
    object$expressions,
    object$baserate,
    object$baserateInflation
  )
  
  class(this.summary) <- "summary.Code"
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

#' @title RegexCode 
#' @description Creates an object for Regular Expression coding.  No need to call this
#' directly, create.code is a nice wrapper around this and any other types of Codes
#'
#' @field name Name of the Code
#' @field definition Definition of the Code
#' @field excerpts Character vector of text excerpts to code
#' @field ... Additional parameters not specific to a RegexCode
#' @field expressions Character vector of regular expressions
#'
#' @examples
#' data(RS.data)
#' rs = RS.data
#' 
#' # Generate a Code
#' newcode = RegexCode$new(name = "New Code", definition = "Some definition", 
#'     excerpts = rs$text, expressions = c("number","data"))
#' 
#' @return RegexCode object
#' @export
RegexCode = R6::R6Class("RegexCode",
  inherit = Code,
                   
  public = list(

    #attributes specific to RegexCode
    expressions = c(),
    metadata = data.frame(),
                     
    ###
    # Main class constructor
    ###
    initialize = function(
      name,
      definition,
      ...,
      excerpts = NULL,
      expressions = NULL
    ){
      super$initialize(name = name, definition = definition, excerpts = excerpts, ...)
      
      if((!is.null(expressions)) && (class(expressions) != "character")){
        stop("expressions must be a vector of strings");
      }
                       
      self$expressions = expressions;
    },
    
    process = function(excerpts = self$excerpts) {
      expression.match(excerpts, self$expressions, names = list(NULL, self$name))
    },
                     
    ###
    # Adds a new expression to the code's list
    ###
    add = function(
      word
    ){
      if(class(word) != "character"){
        stop("word must be a string");
      }
      self$expressions = c(self$expressions, word);
      
      ###DOES THIS AFFECT TRAINING/TEST SETS?
    },
                     
    ###
    # Removes an expression from the code's list
    ###
    remove = function(
      word
    ){
      if(class(word) != "character"){
        stop("word must be a string");
      }
      index = which(self$expressions == word)
      if(length(index) == 0){
        stop(paste("\"", word, "\" does not exist in the expressions list", sep = ""));
      }else{
        self$expressions = self$expressions[-index];
      }
      
      ###DOES THIS AFFECT TRAINING/TEST SETS?
    },
                     
    concat = function(){
      return (paste(self$expressions, collapse="|"));
    }
  ),
  
  private = list()
  
)

expression.match <- function(excerpts, expressions, names = list(NULL, "V1")) {
  matrix(
    unlist(lapply(excerpts, function(x) {
      any(sapply(expressions, function(c) {
        grepl(pattern = c, x = x, perl=T, ignore.case=T)
      }))
    })),
    ncol=1,
    dimnames = names, 
    byrow=T
  ) * 1
}
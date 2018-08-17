#' @title Create CodeSet
#' @description Create a new CodeSet object
#'
#' @param title Title for the CodeSet
#' @param description Description of the CodeSet
#' @param excerpts Set of excerpts to use with the CodeSet
#' @param codes Set of codes to attach to the CodeSet
#'
#' @examples
#' data(RS.data)
#' rs = RS.data
#' code.set = code.set("Demo RS CodeSet", "CodeSet made for the demo", excerpts = rs$text, codes = c())
#'
#' @return CodeSet object
#' @export
###
code.set <- function(title = "", description = "", excerpts = c(), codes = c()) {
  CodeSet$new(title, description, excerpts, codes);
}

#' @title CodeSet 
#' @description Object representing a set of codes
#'
#' @field title Title of the CodeSet
#' @field description String description of the set of codes to be included
#' @field excerpts Character vector of text excerpts to code (optional)
#' @field expressions Codes to include in the CodeSet (optional)
#'
#' @examples
#' data(RS.data)
#' rs = RS.data
#' code.set = code.set("Demo RS CodeSet", "CodeSet made for the demo", excerpts = rs$text, codes = c())
#' 
#' @return CodeSet object
#' @export
#' @return CodeSet
###
CodeSet = R6::R6Class("CodeSet",
  public = list(
    call = NULL,
    
    title = "",
    description = "", 
                        
    excerpts = c(), #character vector containing the set of excerpts that the codes are being applied to
    codes = c(), #collection of codes belonging to the set
  
    ###
    # Main class constructor
    ###
    initialize = function(
      title = "ne",
      description = NULL,
      
      excerpts = c(),
      codes = c()
    ){
      if(class(title) != "character"){
        stop("title must be a string");
      }
      if(class(description) != "character"){
        stop("description must be a string");
      }
                          
      self$title = title;
      self$description = description;
      
      self$excerpts = excerpts;
      self$codes = codes;
    }
  ),
  private = list(
    
  )                
)
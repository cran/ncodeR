#' Create a code
#'
#' @param name Name of the code
#' @param definition Definition of the Code
#' @param excerpts Character vectore of excerpts to use for Coding 
#' @param type Character string representing the type of code (Default: "Regex")
#' @param ... Adiditional parameters
#'
#' @examples
#' data(RS.data)
#' rs = RS.data
#' 
#' # Generate a Code
#' newcode = create.code(name = "Data", expressions = c("number","data"), excerpts = rs$text)
#' 
#'
#' @return Code object
#' @export
create.code <- function(name = "NewCode", definition = "", excerpts = NULL, type = "Regex", ...) {
  newCode = get(paste0(type,"Code"))$new(name = name, definition = definition, excerpts = excerpts, ...)
  return(newCode)
}

# Title
# 
# @param name
# @param definition
# @param codeSet
# @param testSet
# @param trainingSet
# @param computerSet
# @param ignoredSet
# @param examples
# @param excerpts
# @param ...
# 
# @examples
# 
# 
# @return Code object
# @export
Code = R6::R6Class("Code",
  public = list(
    call = NULL,
    name = "",
    definition = "",
    codeSet = NULL,
    excerpts = NULL,
        
    testSet = data.frame(), #matrix, columns: ID, R1training, R1test (with potential to add columns for other raters)
    trainingSet = data.frame(),
    computerSet = data.frame(),
    ignoredSet = data.frame(),
    examples = NULL,
    statistics = NULL,
    baserateInflation = NA,
    baserate = NA,
                     
    ###
    # Main class constructor
    ###
    initialize = function(
      name,
      definition,
      testSet = NULL,
      trainingSet = NULL,
      computerSet = NULL,
      ignoredSet = c(),
      examples = NULL,
      excerpts = NULL,
      ...
    ){
      codeSet = NULL;
      if(class(name) != "character"){
        stop("name must be a string");
      }
      if(class(definition) != "character"){
        stop("conceptual definition must be a string");
      }
      if(!is.null(excerpts)) {
        self$excerpts = excerpts;
        # codeSet = CodeSet$new(title = "NewCodeSet", description = "New CodeSet for Codes", codes = c(self))
      } else if(!is.null(codeSet)) {
        # codeSet$codes = c(codeSet$codes, self)
        if(!is.null(codeSet$excerpts)) {
          self$excerpts = codeSet$excerpts
        } else {
          stop("The codeSet provided doesn't have any excerpts.");
        }
      } else {
        stop("No excerpts found. Use the `excerpts` or `codeSet` params");
      }
                      
      args = list(...);
      
      private[["_id"]] = as.integer( Sys.time() );
      
      # Pre-defined parameters
      self$name = name;
      self$definition = definition;

      if(is.null(testSet)) {
        self$testSet = matrix(ncol = 2, nrow = 0);
        colnames(self$testSet) = c("ID", "1");
      }
      if(is.null(trainingSet)) {
        self$trainingSet = matrix(ncol = 2, nrow = 0);
        colnames(self$trainingSet) = c("ID", "1");
      }
      if(is.null(computerSet)) {
        self$computerSet = rep(NA, length(codeSet$excerpts));
      }

      self$ignoredSet = ignoredSet;
    },
    process = function() {
      stop(paste0("This function needs to be overridden by the implementing Code class: ", class(self)[1]));
    },
    kappa = function(which = c("training","test")) {
      which = match.arg(which, choices = c("training","test"))
      
      to.test.set = self[[paste0(which,"Set")]];
      to.test = cbind(to.test.set,self$computerSet[to.test.set[,1]])
      
      if(nrow(to.test) > 0) {
        rhoR::kappa(to.test[,-c(1)])
      } else {
        NA
      }
    },
    differences = function(data = NULL, col1 = NULL, col2 = NULL, cols = NULL) {
      differences(self)
    },
    
    concat = function(){
      return (paste(self$expressions, collapse="|"));
    },
    print = function() {
      to.print = list();
      ss = get(class(self))
      fields = Filter(function(f) {
        cls = class(newcode[[f]]);
        !is(newcode[[f]], "function") && !is.null(newcode[[f]]) && cls != "environment"
      }, c(names(ss$public_fields), names(ss$get_inherit()$public_fields)))
      for(field in fields) {
        to.print[[field]] = self[[field]]
      }
      print(to.print)
    }
  ),
  private = list(
    "_id" = NULL
  )
)
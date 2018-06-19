#' Title
#'
#' @param x CodeSet to convert
#' @param row.names NULL or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional logical. If TRUE, setting row names and converting column names
#' @param ... additional arguments to be passed to or from methods
#' 
#' @examples
#' data(RS.data)
#' rs = RS.data
#' newcode = create.code(name = "Data", expressions = c("number","data"), 
#'     excerpts = rs$text)
#' code.set = code.set("Demo RS CodeSet", "CodeSet made for the demo", 
#'     excerpts = rs$text, codes = c(newcode))
#' as.data.frame(code.set)
#'
#' @return data.frame
#' @export
as.data.frame.CodeSet <- function(x, row.names = NULL, optional = FALSE, ...) {
  codes = x$codes
  len = length(x$excerpts)
  args = list(...)
  if(is.null(args$len)) len = args$len;
  if(is.null(args$codes)) codes = args$codes;
    
  if(!is.null(len) && len > 0) {
    data.frame(ID = 1:len, excerpt = x$excerpts[1:len], sapply(codes, function(x){ 
      df = data.frame( x$computerSet[1:len] ) 
      colnames(df) = c( x[['name']] )
      df
    }))
  } else {
    data.frame()
  }
}
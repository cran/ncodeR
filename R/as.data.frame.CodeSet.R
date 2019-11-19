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
  len = 0;
  if(is.null(x$excerpts)) {
    excerpts = x$codes[[1]]$excerpts;
  } else{
    excerpts = x$excerpts
  }
  len = length(excerpts)
  args = list(...)
  if(is.null(len) && !is.null(args$len)) len = args$len;
  if(is.null(args$codes)) codes = x$codes;
  if(!is.null(len) && len > 0) {
    coded_cols <- sapply(codes, function(x){ 
      df = data.frame( x$computerSet[1:len] ) 
      df
    })
    names(coded_cols) <- as.character(lapply(codes, `[[`, "name"))
    data.frame(ID = 1:len, excerpt = excerpts[1:len], coded_cols)
  } else {
    data.frame()
  }
}
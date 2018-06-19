#' Title
#'
#' @param x Code object to convert 
#' @param row.names NULL or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional logical. If TRUE, setting row names and converting column names
#' @param ... additional arguments to be passed to or from methods
#'
#' @examples
#' data(RS.data)
#' rs = RS.data
#' newcode = create.code(name = "Data", expressions = c("number","data"), excerpts = rs$text)
#' as.data.frame(newcode)
#' 
#' @return data.frame
#' @export
as.data.frame.Code <- function(x, row.names = NULL, optional = FALSE, ...) {
  len = length(x$excerpts)
  args = list(...)
  if(!is.null(args$len)) {
    len = args$len
  }
  data.frame(ID = 1:len, excerpt = x$excerpts[1:len], sapply(c(x), function(x){ 
    df = data.frame( x$computerSet[1:len] ) 
    colnames(df) = c( x[['name']] )
    df
  }))
}
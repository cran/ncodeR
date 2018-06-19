
Rater.Computer = R6::R6Class("Rater.Computer",
                             public = list(
                                
                               ###
                               # Main class constructor
                               ###
                               initialize = function(data, code){
                                  self$codeData(data, code);
                               },
                               
                               ###
                               # Takes the data and codes it all
                               # Should be run after every single update to data or code
                               ###
                               codeData = function(
                                 data,
                                 code
                               ){
                                  private$seenIndicies = 1:length(data);
                                  private$codingResult = ifelse(grepl((code$concat()),data, ignore.case = TRUE, perl = TRUE), 1,0);
                               },
                               
                               ####
                               #' \code{get()} - Return a read-only property
                               #' \preformatted{  Example:
                               #'     get( x = 'title' )}
                               #' \preformatted{  Parameters:
                               #'      x - Property to return. Defaults to 'title', returning the title}
                               ####
                               get = function(x) {
                                 return(private[[x]])
                               }
                             ),
                             private = list(
                              codingResult = c(),
                              seenIndicies = c()
                             )
)
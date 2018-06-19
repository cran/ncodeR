
Rater = R6::R6Class("Rater",
     public = list(
       
       
       ###
       # Main class constructor
       ###
       initialize = function(
         isRaterOne, 
         availableIndicies
         ){
         private$isRaterOne = isRaterOne;
         private$availableIndicies = availableIndicies;
         private$seenIndicies = c();
       },
       
       ###
       # Adds newly coded data to the rater
       # Does not update the testSetIndex
       ###
       codeData = function(
         indicies, 
         values
         ){
         private$seenIndicies = c(private$seenIndicies, indicies);
         private$handCodingResult = c(private$handCodingResult, values);
       },
       
       ###
       # Called when a test is ran, updates the testSetIndex to the end so all is in the training set
       ###
       updateTestSetIndex = function(){
         private$testSetIndex = length(private$seenIndicies) + 1;
       },
       
       ###
       # Gets the subtraction of available indicies and seen indicies
       ###
       getCodableIndicies = function(){
         if(length(private$seenIndicies) != 0){
          return (private$availableIndicies[-c(private$seenIndicies)]);
         }else{
           return(private$availableIndicies);
         }
       },
       
       ###
       # Adds new indicies that are available for the rater to pull from
       # Only called internally so no need to check indicies for validity
       ###
       addAvailableIndicies = function(
         newIndicies
       ){
         private$availableIndicies = c(private$availableIndicies, newIndicies)
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
       # Indicates whether this rater is the first or second rater
       isRaterOne = TRUE,
       
       # Which indicies of the available indicies the rater has seen
       seenIndicies = c(),
       
       # Which indicies of the entire dataSet are available for the rater to code
       # Available indicies of rater two should be updated when rater one codes more data
       availableIndicies = c(),
       
       # Coding result of the seen indicies
       handCodingResult = c(),
       
       # Index from which testSet starts (Training set before index (exclusive), Test set after index)
       testSetIndex = 0
     )
)
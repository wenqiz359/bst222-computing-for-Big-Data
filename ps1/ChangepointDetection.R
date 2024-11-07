
#' cusumCpt Class for Changepoint Detection Using CUSUM
#' 
#' @description Provides method "addData" to add data, "checkCpt" to check for a changepoint using the CUSUM statistic.
#' 
#' @slot data A numeric vector containing the data points to be analyzed.
#' 
#' @importFrom Rdpack reprompt
#' @references https://en.wikipedia.org/wiki/CUSUM
#' 
#'@examples
#' obj <- cusumCpt(rnorm(100, mean = 0))
#' 
#' @export
setClass(
  "cusumCpt",
  representation = representation(data = "numeric")
)

#' Constructor for cusumCpt Class
#' 
#' Creates a new cusumCpt object for changepoint detection using the CUSUM method.
#' 
#' @param data A numeric vector containing initial data points (default is an empty vector).
#' 
#' @return An object of class cusumCpt.
#' 
#' @importFrom Rdpack reprompt
#' @references https://en.wikipedia.org/wiki/CUSUM
#' 
#' @examples
#' cusum_obj <- cusumCpt(data = c(1, 2, 3, 4, 5))
#' 
#' @export
cusumCpt <- function(data = numeric()) {
  obj <- new("cusumCpt")
  obj@data <- data
  return(obj)
}

#' Add Data to cusumCpt Object
#' 
#' Appends one or more data points to the data slot of a cusumCpt object.
#' 
#' @param obj An object of the class "mycusum".
#' @param data A numeric vector of data points to append.
#' 
#' @return The updated object of the class "mycusum" with new data points added.
#' 
#' @importFrom Rdpack reprompt
#' @references https://en.wikipedia.org/wiki/CUSUM
#' 
#' @examples
#' cusum_obj <- cusumCpt()
#' cusum_obj <- addData(cusum_obj, c(10, 15, 20))
#' 
#' @rdname addData-methods
#' @docType methods
#' @export
setGeneric("addData", function(obj, data) standardGeneric("addData"))
#' @rdname addData-methods
setMethod("addData", signature(obj = "cusumCpt"), function(obj, data) {
  obj@data <- c(obj@data, data)  # Append new data to existing data
  return(obj)
})

#' Detect Changepoint Using CUSUM
#' 
#' Checks if there is a changepoint in the data based on the cusum method and the specified threshold.
#' 
#' @param obj A \code{cusumCpt} object containing the data to be analyzed.
#' @param threshold A numeric value indicating the threshold for the cusum statistic.
#' 
#' @return Logical value: \code{TRUE} if a changepoint is detected, \code{FALSE} otherwise.
#' 
#' @importFrom Rdpack reprompt
#' @references https://en.wikipedia.org/wiki/CUSUM
#' 
#' @examples
#' cusum_obj <- cusumCpt(data = c(1, 2, 3, 4, 5, 50))
#' print(checkCpt(cusum_obj))  # Should return TRUE because of the large jump
#' 
#' @rdname checkCpt-methods
#' @docType methods
#' @export
setGeneric("checkCpt", function(obj,threshold=10) standardGeneric("checkCpt"))
#' @rdname checkCpt-methods
setMethod("checkCpt", "cusumCpt", function(obj,threshold) {
  n <- length(obj@data)
  cusum <- sapply( 1:(n-1), function(i) abs(mean(obj@data[1:i])-mean(obj@data[-(1:i)])) )  # Calculate the CUSUM statistic
  return(any(abs(cusum) > threshold))  # Check if any cusum statistic exceeds the threshold
})



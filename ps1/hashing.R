
setClass(
  "hashing",
  representation=representation(size = "numeric", table = "list")
)

# constructor to start with initial values
hashing <- function(size = 10){
  obj <- new("hashing")
  obj@size <- size
  obj@table <- vector("list", size)
  return(obj)
}

#Define the Hash Function Method
setGeneric("hashfunction", function(obj, key) standardGeneric("hashfunction"))
setMethod("hashfunction", signature(obj='hashing'), function(obj, key) {
  return(key %% obj@size + 1)  # Adding 1 to match R's 1-based indexing
})


#Define the store Method
setGeneric("store", function(obj, key) standardGeneric("store"))
setMethod("store", signature(obj='hashing'), function(obj, key) {
  index <- hashfunction(obj, key)
  if (is.null(obj@table[[index]])) {
    obj@table[[index]] <- list(key)  # Initialize the bucket with the value
  } else if (!(key %in% obj@table[[index]])) {
    obj@table[[index]] <- c(obj@table[[index]], key)  # Add value if not present
  }
  return(obj)
})


#Define the lookup Method
setGeneric("lookup", function(obj, key) standardGeneric("lookup"))
setMethod("lookup", "hashing", function(obj, key) {
  index <- hashfunction(obj, key)
  return(!is.null(obj@table[[index]]) && key %in% obj@table[[index]]) #if there’s anything stored at object@table[[index]].
})





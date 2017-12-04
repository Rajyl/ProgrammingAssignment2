## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function basically stores a matrix 
makeCacheMatrix <- function(x = matrix()) {
  ## makeVector <- function(x = matrix()) {             ## From the original example sited from the assignment.
  varInv <- NULL
  set <- function(y) {
    x <<- y               ## This is called a superassignment "<<-" https://stat.ethz.ch/pipermail/r-help/2011-April/275905.html
    varInv <<- NULL       ## The "<<-" and "<-" yelds a different result. ALWAYS REMEMBER!  
  }
  get <- function() x
  setInv <- function(inverse) varInv <<- inverse
  getInv <- function() varInv
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}

## Write a short comment describing this function
## This function basically computes the inverse of the matrix from the above function.

cacheSolve <- function(x, ...) {
  ## cachemean <- function(x, ...) {              ## From the original example sited from the assignment.
  varInv <- x$getInv()
  if (!is.null(varInv)) {
    message("Getting cached data - Inverted Matrix")
    return(varInv)
  }
  mat <- x$get()
  varInv <- solve(mat, ...)
  x$setInv(varInv)
  varInv
}
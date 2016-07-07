
## makeCacheMatrix and cacheSolve are intented to be used in tandem
## to efficiently manage repeated matrix inversion.
##
## mc <- makeCacheMatrix(mat)
## mSolved <- cacheSolve(mc)
## mSolved2 <- cacheSolve(mc)
## ...
##
## the first call to cacheSolve with a given makeCacheMatrix object,
## the matrix will be inverted and the result cached
##
## on subsequent calls to cacheSolve with the makeCacheMatrix object,
## the cached inversion will be returned

makeCacheMatrix <- function(x = matrix()) {
  # Args:
  #   x: a matrix. Assumed to be invertible
  #
  # Returns:
  #   Object with list of functions:
  #     get() gets matrix
  #     set() sets matrix
  #     getsolve(...) gets inverted matrix
  
  #   verbose: If TRUE, prints notifications. Default is FALSE.
  #       Should be an argument, but that is not what the assignment called for.
  verbose <- FALSE
  
  # the cached solve value.
  solved <- NULL

  # update the matrix
  set <- function(y) {
    x <<- y
    solved <<- NULL
  }

  # return the matrix
  get <- function() x

  # getsolve: the only way to get the inverted matrix
  getsolve <- function(...) {
    
    if (is.null(solved)) {
      if (verbose) {
        message("inverting matrix")
      }
      # Cache the inversion
      solved <<- solve(x, ...)
    } else if (verbose) {
      message("using cached inversion")
    }
    
    solved
  }
  
  list(set = set, get = get,
       getsolve = getsolve)
}


## Get an inverted matrix

cacheSolve <- function(x, ...) {
  # Args:
  #   x: a makeCacheMatrix object
  # Returns:
  #   inverted matrix of x
  
  x$getsolve(...)
}

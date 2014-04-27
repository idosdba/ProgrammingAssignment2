####################################################################
# Description: This file contains two functions that compute the
#   inverse of a matrix for the first time and caches it. Next time
#   the function to get the matrix inverse is called, instead of 
#   computing it again, the cached value is returned thereby avoiding
#   the expensive matrix inverse calculation.
# 1. makeCacheMatrix():  which accepts the input matrix and return
#    a set of functions that set and get the input matrix and the
#    inverse matrix
# 2. cacheSolve():  which returns the inverse matrix that is computed
#    for the first time and a cached value after the first time.
####################################################################


####################################################################
# Function: makeCacheMatrix()
# Accepts a matrix as argument
# Returns a list of functions that set & get the original and 
# inverse matrix.
####################################################################
makeCacheMatrix <- function(x = matrix()) {
  
    # initialize the variable to store the "cached" inverse matrix to NULL
    invMat <- NULL   
  
    #function to initialize the original and inverse matrix
    set <- function(y) {
        x <<- y                 # set the original matrix to the argument
        invMat <<- NULL         # set the inverse matrix to null
    }
  
    # returns the original matrix
    get <- function()  x
  
    # sets the calculated inverse matrix to the "cached" variable, invMat
    setInverse <- function(calcInverseMat) invMat <<- calcInverseMat
  
    # return the "cached" inverse matrix variable
    getInverse <- function() invMat
  
    # return the list of functions
    list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


###################################################################
# Function: cacheSolve()
# Returns the inverse of the matrix received as argument.
# If the inverse already exists (Cached), then returns the 
# cached value, otherwise calculates the inverse, caches it,
# and returns it.
####################################################################
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    inv <- x$getInverse()             # get the "cached" inverse matrix

    # inv is NOT NULL if it was previously calculated and cached
    if (!is.null(inv)) {              
        message("getting cached data")     
        return(inv)                     # return the cached inverse
    }
  
    # inv will be null if the inverse was not calculated and cached
    data <- x$get()                   # get the original matrix data
    inv <- solve(data, ...)           # calculate the matrix inverse
    x$setInverse(inv)                 # cache the calculated inverse
    inv                               # return the calcuated inverse
}

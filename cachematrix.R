## Author:  B. Giordano
## Created: 2014-06-21
## Updated:
## R Programming Class Assignment #2

## Assignment is to use two functions, makeCachedMatrix and cacheSolve, to demonstate scoping 
## and use of a cache to support large data operations - using a cache value, when available,
## rather than recalculating a value that hasn't changed.



## Function:  makeCacheMatrix
## Inputs:    matrix object
## Returns:   list of get/set subfunctions 
## Purpose:   This function creates a "special" matrix object that can cache its inverse.
##            Four subfunctions to access the matrix object are defined:
##              - set (cache) a matrix
##              - get a cached matrix
##              - set (cache) an inverese matrix
##              - get cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    ## initialize the inverse matrix object for x
    inv <- NULL
    
    ## set x (cached matrix obj)
    ## x has been updated so reset the inverse matrix of x to default
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    # return the cached matrix x
    get <- function() x
    
    # set the inverse matrix for x
    # solve is the inverse matrix returned by the solve() function for matrix, x
    setinv <- function(solve) inv <<- solve
    
    # return the cached inverse matrix for x
    getinv <- function() inv
    
    # return list of functions
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function:  cacheSolve
## Inputs:    makeCacheMatrix function
##            additional parameters required by solve()
## Returns:   matrix object
## Purpose:   This function returns the inverse matrix of matrix x.
##            If the inverse matrix exists in the cache, it is returned,
##            otherwise, the inverse is calculated using solve().
##            Both the matrix and its resulting inverse are added to the cache ("special vector")
## NOTE: This function does not address the error condition that an inverse for 
##       a matrix does not exist, per the directions in the programming assignment.

cacheSolve <- function(x, ...) {
  
    ## try get the inverse matrix for matrix passed to makeCacheMatrix(x) (either a matrix is returned or NULL)
    inv <- x$getinv()
    
    ## test if the result returned is valid
    ## if so use it and indicate the result came from the cache
    if(!is.null(inv)) {
      message("Getting cached inverse matrix ...")
      return(inv)
    }
    
    ## establish x in the cache
    data <- x$get()
    
    ## get the inverse of x 
    inv <- solve(data, ...)
    
    ## store the inverse of x to the cache
    x$setinv(inv)
    
    ## return the inverse
    inv
}

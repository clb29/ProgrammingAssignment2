# This file includes a pair of functions that cache the inverse of a matrix
# in order to save processing time as folks can look up vs recompute 
# Note that these functions are valuable if:
# 1. the contents of a matrix do not change AND
# 2. the matrix is large
# Note that functions assume that the supplied matrix is invertible

# makeCacheMatrix:  Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        #initialize local variables
        invx <- NULL  
        
        #set the value of the new global variables (x,invx)
        set <- function (y) {
                x <<- y #?why use same variable names for local and global?
                invx <<- NULL
        }

        #get the value of the matrix
        get <- function() x
        
        #set the value of the inverse matrix
        setinverse <- function(inverse) invx <<- inverse
       
        #get the value of the inverse matrix
        getinverse <- function() invx
        
        #return
        list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)

}

# cacheSolve:  computes the inverse of a special "matrix" returned by makeCacheMatrix 
# Note that IF the inverse has already been calculated (and the matrix has not changed)
# THEN the cachesolve should retrieve the inverse from the cache
# Leverages the solve() which computes the inverse of a square matrix

cacheSolve <- function(x, ...) {
        # determine if inverse already calculated
        invx <- x$getinverse()
        if(!is.null(invx)) {
                message("getting cached data")
                return(invx)
        }
        # when inverse not yet calculated, calculate it and persist in cache...
        data <- x$get()
        invx <- solve(data)
        x$setinverse(invx)

        # Return a matrix that is the inverse of 'x'
        invx
}

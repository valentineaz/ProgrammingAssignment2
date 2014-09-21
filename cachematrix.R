## A pair of functions that cache the inverse of a matrix.

## This function makeVector creates a special "matrix", which is really a list containing a function to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the Inverse of the matrix
## 4) get the value of the Inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 
     In <- NULL
     set <- function(y) {
                x <<- y
                In <<- NULL
        }
        get <- function() x
        
        setInMat <- function(InMat) In <<- InMat
        
        getInMat <- function() In
        
        list(set = set, get = get,
             setInMat = setInMat,
             getInMat = getInMat)
     
}


## The following function calculates the Inverse of the special "matrix" 
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the Inverse
## from the cache and skips the computation. Otherwise, it calculates 
## the Inverse of the data and sets the value of the Inverse in the cache 
## via the setInMat function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        In <- x$getInMat()
        if(!is.null(In)) {
                message("getting cached data")
                return(In)
        }
        data <- x$get()
        In <- solve(data, ...)
        x$setInMat(In)
        In
}

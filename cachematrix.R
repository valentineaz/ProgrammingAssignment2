## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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

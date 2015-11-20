#how do I make this work?
#These functions can store and return the inverse of a matrix.
#This is expensive computationally so the inverse is cached once calculated.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        } #changes the matrix stored in the main function
        get <- function() x #returns the matrix x stored in the main function
        setinv <- function(solve(c)) m <<- mean #store the value of the input
        getinv <- function() m #return the value in setinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv) #stores all the functions
}
}


## cacheSolve should return the inverse of the matrix if it hasn't changed and if it was calculated above
cacheSolve <- function(x, ...) {
        m<- x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return (m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
## Return a matrix that is the inverse of 'x'
#solve(c) %*% c - from me - this should be the function to use
#will this push ever work?
#These functions can store and return the inverse of a matrix.
#This is expensive computationally so the inverse is cached once calculated.
makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        set <- function(y) {
                x <<- y
                v <<- NULL
        } #changes the matrix stored in the main function
        get <- function() x #returns the matrix x stored in the main function
        setinv <- function(solve) v <<- inv #store the value of the input
        getinv <- function() v #return the value in setinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv) #stores all the functions
}

## cacheSolve should return the inverse of the matrix if it hasn't changed and if it was calculated above
cacheSolve <- function(x, ...) {
        v<- x$getinv()
        if(!is.null(v)){ #checks to see if inverse has been calculated
                message("getting cached data")
                return (v)
        }
        data <- x$get()
        v <- solve(data, ...)
        x$setinv(v)
        v
}
## Return a matrix that is the inverse of 'x'


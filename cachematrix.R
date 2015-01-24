## Calculate the inverse of a matrix, chaching the result. 

## Creates a "cachable" matrix object to store a matrix and cache the 
## inverse of the given matrix.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of matrix object returned by makeCacheMatrix function. 
## If the inverse for this matrix has already been calculated, the cached 
## result is returned. Otherwise, the solve function is applied to 
## the matrix and the result is cached.
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}

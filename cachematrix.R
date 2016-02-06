## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    MI <- NULL
    set <- function(y) {
        x <<- y
        MI <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) MI <<- solve
    getinverse <- function() MI
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    MI <- x$getinverse()
    if(!is.null(MI)) {
        message("getting cached data")
        return(MI)
    }
    data <- x$get()
    MI <- solve(data, ...)
    x$setinverse(MI)
    MI
}

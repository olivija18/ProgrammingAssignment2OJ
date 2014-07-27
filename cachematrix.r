# Matrix inversion is long process and it may take a lot of time to compute. The
# following two functions are used to cache the inverse a matrix we specify.

#makeCacheMatrix "set" the value of the matrix/inverse of the matrix and "get" the value of the matrix/inverse of the matrix.
        
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The cacheSolve function returns the inverse of the matrix. It first checks if inversed exists. If yes, it gets it. If not, it computes the inverse sets the inversed value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

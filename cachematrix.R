## The functions create a special matrix with setter and getters and a function that returns a cahced copy of inverse
## or calculates the inverse if the value is not in the cache

## Create a special Matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function (solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setmean = setinv,
         getmean = getinv)

}


##Returns inverse of matrix x. It first looks into cache to find the inve and if not found calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)){
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

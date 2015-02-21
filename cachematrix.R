## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a special matrix object
# cacheSolve calculates the inverse of the matrix
# if the matrix inverse has already been calculated, it will retrieve it
# from the cache and will not calculate it again.

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinverse<- function(inverse) inv_x <<-inverse
        getinverse <- function() inv_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

# cacheSolve returns the inverse of the matrix created with makeCacheMatrix function
# if the cached inverse is available, cacheSolve retrieves it
# if it is not available, it computes and caches it

cacheSolve <- function(x, ...) {
        inv_x <- x$getinverse() # Return the matrix that is the inverse of 'x'
        if (!is.null(inv_x)) {
                message("getting cached inverse matrix")
                return(inv_x)
        } else {
                inv_x <- solve(x$get())
                x$setinverse(inv_x)
                return(inv_x)
        }
}
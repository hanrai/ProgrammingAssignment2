## These two functions are used to compute the inverse of a invertible matrix.
## And it also provide the ability to cache the result. So you don't need 
## to compute the inverse repeatedly.
##
## Example:
## m <- makeCacheMatrix(x)  # First, create a special "matrix" object.
## ...
## r <- cacheSolve(m)  # Get the inverse. 
##


# makeCacheMatrix
# This function creates a special "matrix" object that can cache its inverse.
#
# Args:
#   x: Matrix, it is used to compute the inverse.
#
# Returns:
#   The special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
        # Cache.
        inv <- NULL

        # set(dat), reset the cache matrix object, and clear the inverse.
        set <- function(dat) {
                x <<- dat
                inv <<- NULL
        }

        # get(), Get matrix data.
        get <- function() x

        # setInverse(inverse), Internal function. Set the cached inverse 
        # of matrix by solve function.
        setInverse <- function(inverse) inv <<- inverse

        # getInverse(), Get cached inverse of matrix.
        getInverse <- function() inv

        # Return the special "matrix" object that can cache its inverse.
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


# cacheSolve
# This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.
#
# Args:
#   x: The special "matrix" returned by makeCacheMatrix.
#   ...: Args for solve function.
#
# Returns:
#   Inverse of the special "matrix"

cacheSolve <- function(x, ...) 
{
        # Get cached inverse.
        inv <- x$getInverse()
        
        # Test, return the cached inverse if cache avaliable.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Calculate the inverse if cache is empty.
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        
        # Return the result
        m
}

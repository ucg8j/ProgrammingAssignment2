## The following functions have been written to satisfy the R Programming Coursera course - assignment 3.
## makeCacheMatrix() - creates a special "matrix" object that can cache its inverse.
## cacheSolve() - computes the inverse of the special "matrix" returned by makeCacheMatrix(). 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve() should retrieve the inverse from the cache.

## makdeCacheMatrix() does the following:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## The following function calculates the mean of the special "vector" created with
## makeCacheMatrix. However, it first checks to see if the mean has already been calculated.
## If so, it gets the mean from the cache and skips the computation. Otherwise, it
## calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv() 
    
    # if is not null return cached data
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # get data and solve for inverse
    matrix.data <- x$get()
    inv <- solve(matrix.data, ...)
    
    # update inverse value in cache and return inv
    x$setinv(inv) 
    return(inv)
}
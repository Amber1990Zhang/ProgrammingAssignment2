## Put comments here that give an overall description of what your
## functions do.

## The pair of functions are designed to cache the inverse of a matrix so we 
## don't need to compute it repeatedly.
## 

## Write a short comment describing this function

## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinversion <- function(solve) s <<- solve
        getinversion <- function() s
        list(set = set, get = get,
             setinversion = setinversion,
             getinversion = getinversion)
}

## Write a short comment describing this function

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinversion()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinversion(s)
        s
}


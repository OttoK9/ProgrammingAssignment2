## makeCacheMatrix creates a list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL

        set <- function(y) {
          x <<- y
          m <<- NULL
        }

        get <- function() x
        setinverse <- function(matrix) m <<- matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve solves the inverse of a matrix
## if the inverse has already been solved
## it gets the inverse from the cache and skips the computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()

        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }

        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## These two functions cache the inverse of a matrix

## This function sets the value of the matrix, gets the value of the matrix
## sets the value of the inverse, gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y){
                    x <<- y
                    inv <<- NULL
          }
          get <- function() x
          setinverse <- function(inverse) inv <<- inverse
          getinverse <- function() inv
          list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function checks to see whether the inverse is cached and, if not, returns null

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getinverse()
          if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
          }
          mat <- x$get()
          m <- solve(mat, ...)
          x$setinverse(m)
          m
}

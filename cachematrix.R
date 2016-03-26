##These functions allow you to creae a matrix and cache its inverse so that
##it doesn"t need to be recalculated everytime

##makeCacheMatrix creates a matrix that can cache the inverse value

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL#Inverse matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list (set = set, get = get, setinverse = setinverse,
              getinverse = getinverse)
}


##cacheSolve solves the inverse matrix and caches the value

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {#Returns the inverse if it is aerady cached
                message("Getting the cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
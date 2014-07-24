## Creates a matrix then takes then inverse of it
## The inverse is then cached
## When the inverse is called later on it is retrieved from the cache, if present

## Creates a matrix, then retrieves it
## then its inverse is created and retreived
## the inverse is stored in cache

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) s <<- inverse
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x'
## it retrieves the cached value if it exists
## it does this by referring to the get inverse function 
## the getinverse function is found in the previous function
## if no cached value exists then it performs the inverse calculation

cacheSolve <- function(x, ...) {
                s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}

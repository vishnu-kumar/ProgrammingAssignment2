##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	matrixInverse <- NULL
	set <- function(y) {
                x <<- y
                matrixInverse <<- NULL
        }
	get <- function() x
        setinverse <- function(minverse) matrixInverse <<- minverse
        getinverse <- function() matrixInverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	minverse <- x$getinverse()
        if(!is.null(minverse)) {
                message("getting cached matrix inverse")
                return(minverse)
        }
        data <- x$get()
        minverse <- solve(data)
        x$setinverse(minverse)
        minverse
}

## To run this code first need to create a cacheMatrix object using makeCacheMatrix function
## Now to access the inverse of the matrix, cacheSolve function with the CacheMatrix object as parameter

## This function creates a new type of object which is a cachable matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
	        x <<- y
	        m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## This function solves the given cached matrix, if the cached data is
## present in the elevated environment then that value is returned

cacheSolve <- function(x, ...) {
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

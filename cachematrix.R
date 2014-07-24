## These functions cache the inverse of a matrix

## This function create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,
		 setsolve = setsolve,
		 getsolve = getsolve)
}



## Computes inverse of the matrix returned by above function.
## if inverse of the matrix has already been solved, and 
## matrix has not changed, then this function will search 
## the cache so it does not have to solve it once more.



cacheSolve <- function(x, ...) {
	m <- x$getsolve()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)
	m
}
        ## Return a matrix that is the inverse of 'x'


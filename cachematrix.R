## Put comments here that give an overall description of what your
## functions do

## Creates a cacheable 'matrix' object.

makeCacheMatrix <- function(x = matrix()) {
        invr <- NULL
	set <- function(y) {
		x <<- y
		invr <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) invr <<- inverse
	getinverse <- function() invr
	list( set = set, get = get, setinverse = setinverse, getinverse = getinverse
}


## Calculates the inverse of the matrix object, but only if it has not already been calculated.
## If there is a cached version, this is returned preceeded by the text 'getting cached data'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' - this is 'invr'
        invr <- x$getinverse()
	if(!is.null(invr)) {
		message("getting cached data")
		return(invr)
	}
	data <- x$get()
	invr <- solve(data)
	x$setinverse(invr)
	invr
}

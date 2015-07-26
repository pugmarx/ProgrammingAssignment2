## The first function makeCacheMatrix provides a vector of methods which
## can be called from another environment, and perform the 'caching' operations
## The second function cacheSolve checks if the current matrix has already been supplied
## (in which case it wouldn't need to solve it again, and return the 'cached' version)
## if not, then it proceeds to calculate the inverse and cache it for future use

## Creates a vector of functions which could be called from a separate environment or scope
## and can hold and retrieve the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x 
	setinv <- function(solve) m <<- solve
	getinv <- function() m 

	list(set =  set, get = get, setinv =  setinv, getinv = getinv)
}


# Returns the inverse of the matrix. if the same matrix had been previously passed,
# gets it from the 'cache'. Or else, calculates the inverse and puts it in the cache
cacheSolve <- function(x, ...) {
	m <- x$getinv()
	if (!is.null(m)){
		message("got from cache!")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...);
	x$setinv(m)
	m
}
## These functions create and solve a special matrix object
## that caches its inverse to improve performance.

## makeCacheMatrix
##
## Factory method that creates a special matrix object
##
## Parameter:
##		x - an existing square matrix; if omitted, assumes
##			the value of an empty matrix.
##
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(inverse) {
		inv <<- inverse
	}
	getInv <- function() inv
	list(
		set = set,
		get = get,
		setInv = setInv,
		getInv = getInv
	)
}

## cacheSolve
##
## Calcuates and returns the inverse of a special matrix
## object, passed as a parameter. Caches its result to 
## improve performance.
##
## Parameters:
##		x - matrix to solve; must be an object created 
##			with 'makeCacheMatrix'.
##	  ... - additional parametes passed to 'solve'.
##
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getInv()
	if(!is.null(inv)) {
		message('getting cached matrix inverse')
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setInv(inv)
	inv
}

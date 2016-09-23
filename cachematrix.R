## This file contains 2 functions.
## Function makeCacheMatrix() creates a special matrix object that 
## can cache its inverse. 
## Function cacheSolve() computes the inverse of the special matrix
## returned by makeCacheMatrix(). If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve()
## retrieves the inverse from cache.



## Define function that builds functions set(), get(), setinverse() and 
## getinverse() and returns these functions within a list to the parent
## environment. It also includes the data objects x (cached matrix) 
## and inv (cached inverse).

makeCacheMatrix <- function(x = matrix()) {
	# initialize the inverse to NULL
	inv <- NULL

	# define function to assign a new value to matrix x 
	set <- function(y) {
		x <<- y

		# set the inverse inv to NULL (the inverse has
		# to be computed and set)
		inv <<- NULL
	}

	# define function to retrieve the value of matrix x
	get <- function() {
		x
	}

	# define function to assign a new value to the inverse inv
	setinverse <- function(inverse) {
		inv <<- inverse
	}

	# define function to retrieve the value of the inverse inv
	getinverse <- function() {
		inv
	}

	# return the set and get functions within a named list
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## Define function that retrieves the cached inverse of a matrix, if the
## inverse has already been calculated. Otherwise, it computes and
## caches the inverse.

cacheSolve <- function(x, ...) {
	# retrieve the inverse of x
	inv <- x$getinverse()

	# check if the inverse has not been calculated already
	if(is.null(inv)) {
		# retrieve the matrix value of x
		data <- x$get()

		# compute the inverse of the matrix and assign to x
		inv <- solve(data)
		x$setinverse(inv)
	} else {
		# display message that the calculated inverse is used 
		message("getting cached inverse")
	}

	# return the inverse
	inv
}

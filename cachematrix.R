## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## this function stores an instance of inv and all methods for deal with it.
makeCacheMatrix <- function(mat = matrix()) {
	inv <- NULL						## initialize inv as NULL
	set <- function(new) {				## set a new matrix to mat
		mat <<- new
		inv <<- NULL
	}
	get <- function() mat				## show the actual matrix
	setinv <- function(new) inv <<- new		## push a new value to inv
	getinv <- function() inv			## show the actual value of inv
	list(	set = set, 					## returns the list of methods
		get = get,
		setinv = setinv,
		getinv = getinv )
}

## Write a short comment describing this function

## This function returns a matrix that is the inverse of 'x'
cacheSolve <- function(fct, ...) {			## fct is a makeCacheMatrix list function
	inv <- fct$getinv()				## pull the inverse actual to inv
	if(!is.null(inv)) return(inv)			## if inv not NULL returns inv 
	mat <- fct$get()					## get the matrix from fct environment
	inv <- solve(mat, ...)				## calculate the inverse of mat
	fct$setinv(inv)					## push the new inv to fct environment
	return(inv)						## returns inv
}
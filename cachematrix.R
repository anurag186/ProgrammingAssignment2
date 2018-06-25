## Create functions to cache the inverse of a matrix in memory and retrieve it when needed.


## The purpose of this function is to create an R object that stores a matrix 
## and its inverse in the memory. This function initializes two objects, x 
## and i. x is a matrix object and i is the inverse of matrix x. To get and
## set the matrix, x and its inverse, i, four functions (two get and two set) are
## nested within this function. This function then creates a list object holding
## these get and set functions for the matrix object.
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list (set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function uses an object created using makeCacheMatrix(x) function as an 
## argument and returns the inverse of the matrix, x. Function first attempts to
## check the cached memory to find the inverse of x. If inverse matrix is null,
## function computes the inverse and set the inverse for the makeCacheMatrix 
## object.
cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- inv(data, ...)
	x$setinverse(i)
	i  
}


## execute the script using
## A <- matrix( c(5, 1, 0, 3,-1, 2, 4, 0,-1), nrow=3, byrow=TRUE)
## m <- makeCacheMatrix(A)	## create makeCacheMatrix object in cache
## cacheSolve(m)			## since inverse for m doesn't exist in memory, this will compute the inverse
## cacheSolve(m)			## this will get the inverse from cache.
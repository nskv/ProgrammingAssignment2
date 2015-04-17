## A pair of functions that cache the inverse of a matrix 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL 
	
	# set matrix value
	set <- function( matrix ) {
		mtx <<- matrix 
		inv <<- NULL 
	} 

	# get matrix value
	get <- function() mtx
	
	# set inverse matrix value
	setinverse <- function(inverse) inv <<- inverse
	
	# get inverse matrix value
	getinverse <- function() inv
	
	list(set = set, get = get, 
	     setinverse = setinverse, 
	     getinverse = getinverse) 
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        # is previously computed
	m <- x$getinverse()

        # return previously computed
	if( !is.null(m) ) { 
		message("getting cached data") 
		return(m) 
	} 
	data <- x$get() 
	
	# compute
	m <- solve(data) 
	
	# store computed
	x$setinverse(m)
	
	m 
}

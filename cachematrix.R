## This couple of functions allows to efficiently compute inverse matrix
## with ability to retrieve cached data if input is the same

## How to use:
## > matrix
##      [,1] [,2]
## [1,]    5    1
## [2,]    4    8
## > newMatrix <- makeCacheMatrix(matrix)
## > solve<- cacheSolve(newMatrix)
## > solve
##            [,1]        [,2]
## [1,]  0.2222222 -0.02777778
## [2,] -0.1111111  0.13888889
## > class(solve)
## [1] "matrix"
## > solve<- cacheSolve(newMatrix)
## Getting cached data
## > solve
##            [,1]        [,2]
## [1,]  0.2222222 -0.02777778
## [2,] -0.1111111  0.13888889

## Creates a list with set/get parameters that   
## help distinguish if matrix needs to be
## calculated or take data from cache if cache is not empty

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
	list(set = set, get = get, setInverse = setInverse,
	 	getInverse = getInverse) 
}

## Computes inverse of the first matrix and puts data in cache
## return cached data if same matrix have entered again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
	if(!is.null(m))
	{
		message("Getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}

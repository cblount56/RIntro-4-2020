##Assignment:  Caching the Inverse of a Matrix
##assignment 1 - makeCacheMatrix
##This function will take a square matrix (inverse exists!) and calculate its inverse
##it will cache the results (complete or incomplete)
##after reading discussions I am changing numeric type to table type 022416 08:07 est.
makeCacheMatrix <- function(x = table())	{
		m <- NULL
		set <- function(y)	{
			x <<- y
			m <<- NULL
			}
			get <- function() x
			setinverse <- function(x) m <<- solve(x) 
			getinverse <- function() m
			list(set = set, get = get,
				setinverse = setinverse,
				getinverse = getinverse)
}
##end assignment


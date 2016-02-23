##Assignment:  Caching the Inverse of a Matrix
##assignment 1 - makeCacheMatrix
makeCacheMatrix <- function(x = numeric())	{
		m <- NULL
		set <- function(y = numeric())	{

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


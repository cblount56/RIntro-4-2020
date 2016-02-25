##Assignment:  Caching the Inverse of a Matrix
##assignment 1 - makeCacheMatrix
##This function will take a square matrix (inverse exists!) and calculate its inverse
##it will cache the results (complete or incomplete)
##I wrote a matrix construction function named matrixIn which takes one argument the, the number of
##rows or columns (row=column) and creates a mtrix filled with randomly generated numbers.
##code of matrixIn  (executed before calling makeCacheMatrix)
##	matrixIn <- function(a) {
##		len= a*a
##		x <<- matrix(rnorm(1:len), a, a)
##this can be handled by the argument you provide to call 'makeCacheMatrix' but I needed a matrix 
##set of values that I could check.
makeCacheMatrix <- function(x)	{
		m <- NULL
		set <- function(y)	{
			x <<- y
			m <<- NULL
			}
			get <- function() 
			setinverse <- function(solve) m <<- solve(x) 
			getinverse <- function() m
			list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
##end assignment


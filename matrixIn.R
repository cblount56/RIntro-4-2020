##create a matrix of arbitrary of square dimensions (a x a) populated with random numbers
				matrixIn <- function(a)	{
				len = a*a
				x <<- matrix(rnorm(1:len), a, a)
				}

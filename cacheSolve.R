##cacheSolve
##This function will calculate or retrieve from cache the inverse of a matrix
##produced by R function named 'makeCacheMatrix.R'
##comments added 022416 08:11 est
cacheSolve <- function(x, ...) {
##error msg >Solve(b)
##Error in x$getinverse : $ operator is invalid for atomic vectorsmakeCacheMatrix(b
        m <- x$getinverse()
        if(!is.null(m)) {
              message("getting cached data")
              return(m)
        }
        data <- x$get()
        m <- solve(data, ...)list
        x$setinverse(m)
        m
}
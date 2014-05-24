## R functions makeCacheMatrix and cacheSolve are based on 
## functions provided by Roger D. Peng, PhD as part of the
## 2014 R Programming Coursera course.

## Use: create a matrix to be inverted
##      >c=rbind(c(1, -1/4), c(-1/4, 1))  

##      Run makeCacheMatrix to create a list of functions
##      >w<-makeCacheMatrix(c)

##      Run cacheSolve on the list of functions created by 
##      makeCacheMatrix to either calculate the inverse or 
##      retrieve the cached inverse if exists.

##      >cacheSolve(w)
##                [,1]      [,2]
##      [1,] 1.0666667 0.2666667
##      [2,] 0.2666667 1.0666667

##      If results are cached the output is
##                [,1]      [,2]
##      [1,] 1.0666667 0.2666667
##      [2,] 0.2666667 1.0666667

makeCacheMatrix <- function(x = numeric()) {
        ## Clear out m
        m <- NULL
        ## Define set
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                
                ## Notify that the cached inverse 'x' is being retrieved and is then returned.
                
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
        m
}
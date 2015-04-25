## This function creates a matrix and calculates and stores the inverse of the
## matrix in cache for faster future computations


## The first function, makeCacheMatrix creates a special "matrix", which is 
## really a list containing a function to
## 1) set the value of the vector
## 2) get the value of the vector
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) { ## Changed from x = vector to x = matrix
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinverse <- function(solve) m <<- solve ## Changed fucntion to solve
                getinverse <- function() m
                list(set = set, get = get,
                     setinverse = setinverse,  ##changed from setmean to setinverse
                     getinverse = getinverse)  ##changed from getmean to getinverse
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

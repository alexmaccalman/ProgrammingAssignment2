## This function makes a special matrix and stores it in a cache
## the function creates a list that contains the following functions
## 1. set teh vaue of the matrix
## 2. get the value of the matrix
## 3. set the result of the inverse
## 4. get the result of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <-NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calcualtes the inverse of the special matrix created in 
##makeCacheMatrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

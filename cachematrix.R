## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## x is a matrix
## return a list containing functions to:
## 1. set the matrix, 2. get the matrix, 3. set the inverse, 4. get the inverse
## use `<<-` to assign a value to an object in an environment different from the current environment. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
## Return a matrix that is the inverse of 'x'
## if the inverse has already been calculated, get from cache and skip computation
## inverse calculated if not in cache and sets the value

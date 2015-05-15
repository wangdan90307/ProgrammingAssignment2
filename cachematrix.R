## Put comments here that give an overall description of what your
## functions cache the inverse of a matrix

## Write a short comment describing this function
## input: nothing
## output: a list of functions, that will do:
## 1. set(): set the matrix
## 2. get(): get the matrix
## 3. setinverse(): set the inversed matrix
## 4. getinverse(): get the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function(){
        x
    }
    setinverse <- function(inv){
        inverse <<- inv
    }
    getinverse <- function(){
        inverse
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## input: the returned list from the makeCacheMatrix function
## output: inversed matrix (the inversed matrix will also be cached)
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'    
    inverse <- x$getinverse()
    if (!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data,...)
    x$setinverse(inverse)
    inverse
}

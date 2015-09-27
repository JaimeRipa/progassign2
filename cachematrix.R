## Put comments here that give an overall description of what your
## functions do

## As the sample, makeCacheMatrix creates a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
        in <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) in <<- inverse
        getinverse <- function() in
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function

## The function CacheSolve returns the inverse of the matrix x.
## First checks if the inverse is in memory. If TRUE, gets the results without computation.
## If FALSE, computes the inverse and saves the result in cache via setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        in <- x$getinverse()
        if(!is.null(in)) {
                message("getting cached data.")
                return(in)
        }
        data <- x$get()
        in <- solve(data)
        x$setinverse(in)
        in

        }

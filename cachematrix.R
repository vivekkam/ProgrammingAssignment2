## Put comments here that give an overall description of what your
## functions do
## The function set,get,setinverse and getinverse: Sets the matrix, Gets the matrix
## Sets the inverse and Gets the inverse respectively.
## Write a short comment describing this function
## The function, makeCacheMatrix creates a special "matrix" which is a list 
## containing function to
## 1 Set the matrix
## 2 Get the matrix
## 3 Set the inverse
## 4 Get the inverse

makeCacheMatrix <- function(x = matrix()) {
	  i <- NULL
        set <- function(y) {
                x <<- y    
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse 
        getinverse <- function() i
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse is already calculated, 
## It gets the result from the cache else the inverse if calculated using the solve function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
           i <- x$getinverse()
        if(!is.null(i)) {
                           message("getting cached data")
                           return(i)
                         }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        return(i)
}

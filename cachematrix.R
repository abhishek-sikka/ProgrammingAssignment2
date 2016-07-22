## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix which is nothing but a list containing getter and setter 
## methods for the base matrix as well as its inverse

makeCacheMatrix <- function(x = matrix()) {
        
          inv <- NULL
          
          set <- function(temp){
                x <<- temp
                inv <<- NULL
          }
          
          get <- function()
                x
          
          setInverse <- function(inverse)
                inv <<- inverse
          
          getInverse <- function()
                inv
          
          list (set = set, get = get, setInverse= setInverse, getInverse = getInverse)
          
            
}


## This method takes a "special" matrix as an argument and returns its inverse using solve
## When returning the inverse, it also sets the inverse in the special vector using the setInverse method
## This acts as a cache and using the null check, we return from the cache if the inverse has been calculated once already

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        if(!is.null(inv)){
              message("Restoring from cache")
              return(inv)
        }
        
        data <- x$get()
        inv <- solve(data,...)
        x$setInverse(inv)
        inv
}

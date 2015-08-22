## Here I describe what these functions do.
## My code is intended to cache the inverse of a matrix.

## This function gets, sets, and outputs the value

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  
  set <- function(y) {  
    x <<- y  
    m <<- NULL  
  }  
  get <- function() x  
  setInverse <- function(inverse) m <<- inverse  
  getInverse <- function() m  
  list(set = set, get = get,  
    setInverse = setInverse,  
    getInverse = getInverse)  
}


## This function computes the inverse if it hasn't 
## already been computed, else it provides cached version

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m <- x$getInverse()  
	if(!is.null(m)) {  
  	message("fetching cached data")  
	  return(m)  
	}  
	data <- x$get()  
	m <- inverse(data, ...)  
	x$setInverse(m)  
	m  
}

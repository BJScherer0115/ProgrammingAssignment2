## a pair of functions that cache the inverse of a matrix.
##
##Update 4/22/2015 - BJS added code to the shell 

## Example execution
## > m <- makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2)))
## > cacheSolve(m)
## Calculate the inverse
## [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5
## > cacheSolve(m)
## Retrieving from the Cache
## [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5

## Create a special "matrix", which is a list containing
## a function to
##   1 - set the value of the matrix
##   2 - get the value of the matrix
##   3 - set the value of the inverse matrix
##   4 - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  #Set the index to Null
  inv <- NULL
  #set function
  set <- function(y) {
    #Set the Closure       
    x <<- y
    inv <<- NULL
  }
  #get function
  get <- function() x
  #set the inverse
  setinverse <- function(inverse) inv <<- inverse
  #get the inverse
  getinverse <- function() inv
  #the Special Matrix 
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Calculate the ("inv") inverse of the special "matrix" created with 
## makeCacheMatrix function, 
## Reusing the cached result if it is available for efficiency
## Return(Inv) 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  #determine if the cache has what is needed
  if(!is.null(inv)) {
    message("Retrieving from the Cache")
    return(inv)
  }
  else {
    message("Calculate the inverse")  
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    return(inv)
  }
}

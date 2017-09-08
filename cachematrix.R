## R Programming Assignment 2
## Cache's the inversion of a matrix that can be looked up in subsequent runs
## Reduces computational effort

## makeCacheMatrix makes a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Returns a list that is used as input into casheSolve
  
  inv = NULL
  set = function(y) {
    
    x <<- y
    inv <<- NULL
  }
  
  get = function() x
  setInv = function(inverse) inv <<- inverse
  getInv = function() inv
  list(set=set, get=get, setInv=setInv, getInv=getInv)

}


## Cretes the inverse of the matrix
## But first checks to see if the inverse has been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv = x$getInv()
  ## if the inverse has already been calculate, return the following message
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  ## If no cached data, calculate the inverse
  data = x$get()
  inv = solve(data,...)
  x$setInv(inv)
  return(inv)
  
}

### Testing the function
testing = function(matrix) {
  
  temp = makeCacheMatrix(matrix)
  startTime = Sys.time()
  cacheSolve(temp)
  duration = Sys.time() - startTime
  print(duration)
  
  startTime = Sys.time()
  cacheSolve(temp)
  duration = Sys.time() - startTime
  print(duration)
  
}

## create a matrix to test the function
r = rnorm(1000000)
matrix1 = matrix(r, nrow=1000, ncol = 1000)
testing(matrix1)

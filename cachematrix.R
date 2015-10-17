## create a list of options for cacheSolve
## functions do: set, get, setInv and getInv
## uses <<- assig. op for internal variables 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## x is a square invertible matrix
  ## return: a list containing functions to work on matrix
  ##    set the matrix and  get the matrix
  ##    set the inverse & get the inverse
  ## basic utils for cacheSolve()
  
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    # A setter function, use this to set a matrix to object created by makeCacheMatrix function
    # e.g makeCacheMatrix$set(testmatrix) # here we work on testmatrix
    #     makeCacheMatrix$set(testmatrix1) # here we work on testmatrix1
    x <<- y
    inv <<- NULL 
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}



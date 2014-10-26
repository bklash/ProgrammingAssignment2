## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##Creates a new variable in the parent environment of makeCacheMatrix
  m <- NULL
  set <- function(y) {
    ##x defined as an input from makeCacheMatrix function
    x <<- y
    ##NULL will be updated in the use of setmean function
    m <<- NULL
  }
  ## This type of notification is same as function () {x}
  get <- function() x
  ##Sets m as the inverse matrix of x
  setinverse <- function(solve) m <<- solve
  ##Value of m is taken from getinverse function
  getinverse <- function() m
  ## This returns a list of outputs from this function for the user to retrieve if needed
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## This line gets the inverse from getinverse function from previous function
  m <- x$getinverse()
  ##Tests if m is null or not, if not null then returns the value. If null it goes on to calculate Inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ##computes inverse from the data
  m <- solve(data, ...)
  ## Extracts setinverse function, which updates m in the parent env
  ## setinverse <- function(solve) m <<- solve
  x$setinverse(m)
  ## prints the value of m
  m
}




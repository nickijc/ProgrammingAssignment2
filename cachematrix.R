## These functions demonstrate caching using the <<- operator and scoping rules in R.
## <<- is used to assign a value to an object, preserving its state within the scope of the function (i.e. cacheing it), so that it may be retrieved when
## required rather than having to recompute it.

## "makeCacheMatrix" creates a special matrix and defines a function to
## set the matrix, get the matrix, set the inverse & get the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # set and store the matrix 
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  # retrieve the stored matrix
  get <- function() x
  # sets  
  setinv <- function(solve) i <<- solve
  #retrieve the stored inverse
  getinv <- function() i
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}


## "cacheSolve" calculates the inverse of the special matrix. If it has already been calculated, it will retrieve the stored result,
## otherwise it will use the setinv fn to calculate and store it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## First check if the inverse has already been calculated and stored. 
        i <- x$getinv()
        if (!is.null(i)){
             #the inv has been previously cached
             message("getting cached data")
             return(i)
        }
        # new calcultion - get the matrix value and calculate inverse
        data <- x$get()
        i <- solve(data, ...)
        #cache the inverse
        x$setinv(i)
        i
        
}

## To test:
## mat <- matrix(4:1, 2,2)          defines an invertible matrix
## z<- makeCacheMatrix(mat)         create the special matrix
## z$get()                          displays the created matrix
## cacheSolve(z)                    1st time through will calculate the inverse
## cacheSolve(z)                    Subsequently retrieves the inverse from the cache.

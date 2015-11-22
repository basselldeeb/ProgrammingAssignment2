##The goal of this code is to compute the inverse of a matrix if it has not already been calculated
##If it has already been calculated, then the code will refer to the cache to identify the values

##The makeCacheMatrix function will create a special "matrix" objects that can cahce its inverse
makeCacheMatrix <- function(x = matrix()) 
  {
  invMat <- NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invMat <<- inverse
  getinverse <- function() invMat
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.  
## if the inverse has already been computed, it will be retrieved from the cache. 


cacheSolve <- function(x, ...) 
  {
  invMat <- x$getinverse()
  if(!is.null(invMat)) 
    {
    message("getting cached data!")
    ## Return a matrix that is the inverse of 'x'
    return(invMat)
    }
  data <- x$get()
  invMat <- solve(data)
  x$setinverse(invMat)
  invMat
        
  }

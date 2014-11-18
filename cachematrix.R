## makeCacheMatrix takes an input x, which is a matrix.  The output is a list.  
##Subsequently cacheSolve uses the list created for either computing the inverse (if not already computed) or
## getting the cahced value of the inverse if already computed

## creates objech which will be used by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y){
  x <<- y
  i <<- NULL
}
get <- function() {x}
setinv <- function(inverse){i<<-inverse}
getinv <- function() {i}
list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Uses the object created by makeCacheMatrix and either calculates inverse or just get the cahce value of inverse.

cacheSolve <- function(x, ...) {
  i<-x$getinv()
  if (!is.null(i)){
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
  
        ## Return a matrix that is the inverse of 'x'
}

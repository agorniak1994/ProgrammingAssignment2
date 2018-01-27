## Cache inverse function  - save the inverse of a matrix to access it later without having to redo the calculation
## Consisting of two functions: makeCacheMatrix & cacheSolve

## Sets the matrix to be inverted, sets the inverse and enables to return it 

makeCacheMatrix <- function(x=matrix()){
  #initiate destination object
  inverse <- NULL
  #define original object
  set <- function(y){
    x<<-y
    inverse<<-NULL
  }
  #return original object
  get <-function() x
  #save the inverse of original object to initiated destination object
  setinverse <- function(inv) inverse<<-inv
  #return inverse function
  getinverse <- function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Checks whether inverse has already been calculated: if so, inverse is returned, otherwise it is calculated and stored

cacheSolve <- function(x,...){
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  help <- x$get()
  inverse <- solve(help)
  x$setinverse(inverse)
  inverse
}





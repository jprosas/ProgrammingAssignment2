## The function makeCacheMatrix receives an invertable matrix 
## from it, it creates a special Matrix in a similar way as the makeVector example provided
## however the information store in this list is the matrix and its inverse
## the matrix can be read from the list using get
## while the inverse of said matrix can be written using setinv or read using getinv

## The function cacheSolve receives a special Matrix created by makeCacheMatrix
## it then iquires if the inverse info has been store in the cache
## if so it uses x$getinv() to get the the stored data
## on the contrary it calculates the inverse of the matrix using solve(x,...)
## and stores it in the cache using x$setinv(inv)

## A working example will be the following:
## >  x<-x<-matrix(1:4,2,2)
## >  xSpecial<-makeCacheMatrix(x)
## >  cacheSolve(xSpecial) #first run calculates
## >  cacheSolve(xSpecial) #second reads cached data

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,setinv = setinv,getinv = getinv)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  m<-x$get()
  inv<-solve(as.matrix(m))
  x$setinv(inv)
  print(inv)
}
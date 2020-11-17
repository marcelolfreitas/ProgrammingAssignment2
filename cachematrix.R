## Put comments here that give an overall description of what your
## functions do

## This function below creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(m){
    x<<-m
    inv<<-NULL
  }
  get<-function()x
  setinv<-function(inverse)inv <<- inverse
  getinv<-function()inv
  list(set=set, get=get, setinv = setinv, getinv = getinv)

}


## This function computes the inverse of the special matrix returned by makeCachedMatrix above.
## If the inverse has already been calculated, then chaeSolve retrive the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matr<-x$get()
  inv<-solve(matr,...)
  x$setinv(inv)
  inv
}


##cachematrix.R provides functions to cache the inverse of a matrix object
#
#Example:
#
#m<-matrix(runif(3*3),3,3) 
#mc<-makeCacheMatrix(m) 
# cacheSolve(mc)
#[,1]      [,2]      [,3]
#[1,] -3.277591 -0.560498  1.929967
#[2,]  2.278223 -3.290088  1.516839
#[3,]  2.189629  2.498472 -1.819851
#
#cacheSolve(mc)
#getting cached data
#[,1]      [,2]      [,3]
#[1,] -3.277591 -0.560498  1.929967
#[2,]  2.278223 -3.290088  1.516839
#[3,]  2.189629  2.498472 -1.819851

##makeCacheMatrix
#
#Extends a matrix object with a set of functions to cache the inverse of it#
#set(y)     : set the matrix to another matrix y and sets its inverse to NULL
#get()      : return the default matrix object
#setinv(inv): sets the inverse of the matrix to inv
#getinv()   : returns the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){x}
  setinv<-function(inv){i<<-inv}
  getinv<-function(){i}
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

##cacheSolve
##
##Provides the default solve function for an extended matrix object created
##with the mackeCacheMatrix function.
##If the inverse was already calculated cacheSolve returns the cached inverse
##otherwise the default solve function will be called.
cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat, ...)
  x$setinv(inv)
  inv
}
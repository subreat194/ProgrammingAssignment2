# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * setMat      set the value of a matrix
# * getMat      get the value of a matrix
# * cacheInverse   get the cahced value (inverse of the matrix)
# * getInverse     get the cahced value (inverse of the matrix)
#
##
## Functions that cache the inverse of a matrix
##
## > source('cacheMat.R')
## > m <- makeCacheMatrix(matrix(c(2, 4, 2, 8), c(2, 2)))
## > cacheSolve(m)
##     [,1]  [,2]
##[1,]  1.0 -0.25
##[2,] -0.5  0.25

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setmat<-function(solve)
  m<<- solve
getmat<-function() m
list(set=set, get=get,setmat=setmat,getmat=getmat)
}

## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available

cacheSolve <- function(x=matrix(), ...) {
  # get the cached value
    m<-x$getmat()
    # if a cached value exists return it
    if(!is.null(m)){
      message("get cached data")
      return(m)
    }
    # otherwise get the matrix, caclulate the inverse and store it in
    # the cache
    mat<-x$get()
    m<-solve(mat, ...)
    x$setmat(m)
    # return the inverse
    m
}

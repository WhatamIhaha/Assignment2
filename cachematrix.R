## The first function sets a matrix and it returns a list of functions the second function tries to find
## inverse matrix of the output of the first function and if it is in the cache, it would not calculate it agian
## but directly use the existing result

## to return the list of functions required

makeCacheMatrix <- function(x = matrix()){
  inv = NULL
  store<-NULL
  storeinv<-NULL
  set = function(givematrix) {
    store <<- givematrix
    inv <<- NULL
  }
  get<- function() store
  setinv<- function() storeinv<<- solve(store)
  getinv<- function() storeinv
  list(set=set,get=get,setinverse=setinv,getinverse=getinv)
}  

## return the inverse matrix of the output from the first function

cacheSolve <- function(store, ...) {
  inv = store$getinv()
  if (!is.null(storeinv)){
      # get it from the cache and skips the computation 
      message("getting cached data")
      return(storeinv)
  }
  #calculating the inverse 
  matrix= store$get()
  inv = solve(matrix, ...)
  store$setinv(inv)
  
  return(inv)
}
    

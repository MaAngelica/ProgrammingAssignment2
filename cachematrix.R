# We use this function when we need to compute something that spend so many time. We can cache rather than compute it repeatedly.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse


makeCacheMatrix<-function(x=matrix()) {
  inversa<-NULL
  set<-function(y){
    x<<-y
    inversa<<-NULL
  }
  get<-function()x
  setinversa<-function(solve) inversa<<-solve
  getinversa<-function() inversa
  list(set=set, get=get, setinversa=setinversa, getinversa=getinversa)
}

## cacheSolve function calculates the inverse of the special "matrix" created with the above function.
cacheSolve<-function(x, ...){
  
  inversa<-x$getinversa()
  if(!is.null(inversa)){
    message("Getting cached data")    
    return(inversa)    
  }
  data<-x$get()
  inversa<-solve(data,...)
  x$setinversa(inversa)
  inversa
}

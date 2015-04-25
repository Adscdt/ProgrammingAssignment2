## Creates matrix cache to cahe input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
  set<-function(y){
    x<<-y 
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,                        
       setmatrix=setmatrix,
       getmatrix=getmatrix)     ##List to store 4 member functions : set, get, setmatrix, getmatrix
}


## To get the inverse matrix from object x
cacheSolve <- function(x, ...) {
m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
        
}

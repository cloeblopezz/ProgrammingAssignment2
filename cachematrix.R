remove(list=ls())

makeCacheMatrix<-function(x= matrix()) {
  
      m<-NULL
      set<-function(y){
        x<<-y
        m<<-NULL
        
      }
      get<-function()x
      setinvert<-function(invert) m<<-solve
      getinvert<-function() m
      list(set = set, get = get,
           setinvert = setinvert,
           getinvert = getinvert)
      
  
}

cacheSolve<-function(x, ...){
  
  m<-x$getinvert()
  if(!is.null(m)) {
    
    message("Getting cached data")
    return(m)
  }
  
  data<-x$get()
  
  m<-solve(data, ...)
  x$setinvert(m)
  
  m
  
  
}

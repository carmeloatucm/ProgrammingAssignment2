  ### before running cache matrix it is convenient to dub
  ### the matrix you're introducing as x
  ### making reference that it may be defined elsewhere
  ### for instance if you're using a for loop and 
  ### at each step you want to get the inverse of matrix[[i]]
  ### run x<<- matrix[[i ]] at the start of the loop.

makeCacheMatrix <- function(x =matrix()){
    
 
  
  
  m <- NULL
  ### initially m starts as a NULL object, it will be tested later on
  ### if it has been calculated before 

  set <- function(y){
      x <<-y
      m<<-NULL
    }  
  ### this function takes a matrix and calls it x
  ### and yields m if m is properly defined elsewhere is not NULL
    
  get <- function( ) x
  ### this function presents the matrix x that yields the 
  ### argument defined by ther function set
    
  setinv <- function(solve) m <<- solve
  ### takes the solve funtion to get the inverse matrix
    
  getinv <- function() m
  ### yields the object m, the inverse of x as defined by 
  ### the function set
  
  ### The outcome is a list of functions that are used to get a
  ### cache to be tested later on
  
  list(set=set, get=get,setinv=setinv,getinv=getinv)
}

### the next function gets the inverse of a matrix after checking 
### whether it has been already calculated and in the cache.

cacheSolve<- function(x,...){
  m <- x$getinv()
  #### have we already calculated m?
  if(!is.null(m)){
        message("getting cached data")
        return(m)
  }
  ### if we already have it, we get it from cache,
  ### if not we proceed to define it and introduce it in the cache
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}

### In a for loop to calculate solve(matrix[[i]])
### we should operate these functions as
### x <<-matrix[[i]]
### matrixinv[[i]] <- cacheSolve(makeCacheMatrix(x))
### 

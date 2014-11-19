  ### before running cache matrix it may be convenient to dub
  ### the matrix you're introducing as x
  ### making reference that it may be defined elsewhere
  ### for instance if you're using a for loop and 
  ### at each step you want to get the inverse of matrix[[i]]
  ### run x<<- matrix[[i ]] at the start of the loop.

makeCacheMatrix <- function(x =matrix()){
  data <- NULL
  ### initially the data starts as a NULL object, it will be tested and cached later on
  
  set <- function(y){
    x <<-y
    data<<-NULL
  }  
    ### this function takes a matrix and calls it x
    ### and yields data if data is properly defined elsewhere is not NULL
  get <- function( ) x
    ### this function presents the matrix x that yields the 
    ### argument to be tested with the data defined  by ther function set
  setinverse <- function(solve) inverse <<- solve
    ### takes the solve funtion to get the inverse matrix
  getinverse <- function() inverse
    ### yields the functions that gets the inverse matrix 
  list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
    ### The outcome is a list of functions that are used to get a
    ### cache to be tested later on
  }



cacheSolve<- function(x,...){
  inverse <-x$getinverse()
      ### gets the inverse matrix from the cached matrix if any defined
  if(!is.null(inverse) & identical(x$set,x$get)==TRUE){
    message("getting cached data")
    return(inverse)
  }
      ### checks whether the input matrix and the cached matrix are identical
      ### and whether the inverse has been already calculated
      ### if both ifs are positive, the inverse matrix is returned
      ### if negative the function proceeds to get the inverse from input matrix
      
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  x$set(data)
      ### and gets the input matrix and its inverse matrix to the cache 
  inverse
  
}

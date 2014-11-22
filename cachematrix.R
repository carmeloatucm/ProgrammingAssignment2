  ### This function stores the inverse and the value of the matrix for future use

makeCacheMatrix <- function(x =matrix()){
 ### initially the data starts as a NULL object, it will be tested and cached later on
 inverse <- NULL
   
 data <-NULL
  
set <- function(y){
   x <<- y
   inverse <<-NULL
 }
  
  get <- function( ) x
  
  setinverse <- function(solve) inverse <<- solve
  ### takes the solve funtion to get the inverse matrix
  
  getinverse <- function() inverse
  ### yields the functions that gets the inverse matrix 
  
 
 
 set2 <- function(z){
   x <<- z
   data <<-NULL
 }
 
  setcached <- function(identity) data <<- identity
  getcached <- function() data
  
  
  
  
  list(set=set,
    get=get,setinverse=setinverse,
       getinverse=getinverse, set2=set2, setcached=setcached, getcached=getcached)
  ### The outcome is a list of functions that are used to get a
  ### cache to be tested later on
}



cacheSolve<- function(x,...){
  inverse <-x$getinverse()
  ### gets the inverse matrix from the cached matrix if any defined
  if(identical(x$getcached() ,x$get())==TRUE){
message("getting cached data")
    return(inverse)
  }
  ### checks whether the input matrix and the cached matrix are identical
  ### and whether the inverse has been already calculated
  ### if positive, the inverse matrix is returned
  ### if negative the function proceeds to get the inverse from input matrix
  
  data <- identity(x$get())
  inverse <- solve(data)
  x$setinverse(inverse)
  x$setcached(data)
  ### and gets the input cached matrix and its inverse matrix to the cache 
  inverse
}




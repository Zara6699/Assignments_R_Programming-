## Put comments here that give an overall description of what your
## functions are doing
#> These two functions makeCacheMatrix and CacheSolve are going to help us to save plenty of time 
#and avoiding to computation from scratch over and over. 

## Write a short comment describing this function:
#> By means of makeCacheMatrix function, the phase of "settings" and "gettings" are definded below: 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <- y
    m <- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}

## Write a short comment describing this function:
#> CacheSolve function is recalling any previous comuptation, if there is any, otherwise, it 
# starts the computation from the begining and keep the result for future reference. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse ()
    if(!is.null(m)) { 
      message("getting cached data")
      return(m)
     }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


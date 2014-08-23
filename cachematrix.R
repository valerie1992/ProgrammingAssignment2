## Write a pair of functions that cache the inverse of a matrix.
## Also, when calculating the inverse, it first checks to see if
## the inverse has already been calculated. If so,it gets the inverse
## from the chache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse in the cache
## via the setinverse function.



## 1. Set the value of the matrix;
## 2. Get the value of the matrix;
## 3. Set the value of the inverse;
## 4. Get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {## input x will be a matrix
  m<-NULL   ## m will be our "inverse" and it's reset to NULL every time makeCacheMatrix is called
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x     ## This function returns the value of the original matrix
  setinverse<-function(solve)m<<-solve ## This is called by cacheSolve() during the first cacheSolve() access and it will store the value
  getinverse<-function()m  ## This will return the cached value to cacheSolve() on subsequent accesses
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}



## When called, it will see if the inverse has been stored. If not, it will calculate the inverse, store it and then return it.
## If the inverse for the data has been calculated and stored earlier, it will fetch the inverse and return it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse() ## accesses the data "x" and gets the value of the inverse
  if(!is.null(m)){  ## if the inverse was already cached (not NULL)
    message("getting cached data")  ## send this message
    return(m)  ## return the inverse
  }
  data<-x$get()  ##if the inverse is not calculated before (x$getinverse() returned NULL)
  m<-solve(data,...)  ## if m was NULL, then we calculate the inverse
  x$setinverse(m)  ## store the calculated inverse value in x
  m  ## return the inverse
}


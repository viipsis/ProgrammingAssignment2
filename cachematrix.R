## Does calculations once for the inverse for given data
## and stores them to the cache for easy retrieval.

## makeCacheMatrix - does inverse calculations and caches them
## in case they are needed again
## (Utilizes the idea of makeVector)
## The Cached values of x can be obtained when this function
## is run first.

makeCacheMatrix <- function(x = matrix()) {

  ex<-NULL
  
  # basic value settings
  set<-function(y){
    x <<-y
    ex<<-NULL
  }
  get<-function() x
  
  # the inverse calculation
  setinverse <- function(solve) ex <<- solve
  getinverse <- function() ex
  
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)

}


## cacheSolve - does the inverse computation utilizing cache
## if cache doesn't contain the data does the computation and stores
## then the result

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  ex <- x$getinverse()
  
  if(!is.null(ex))
  {
    message("Cache found, yay!")
    return(ex)
  }

  result<-x$get()  #in this case inverse

  #print("???")
  #print(result)
  
  ex<-solve(result,...)
  x$setinverse(ex)
  
  return(ex)  
}

## test: https://class.coursera.org/rprog-013/forum/thread?thread_id=127
# ***********************************
# m <- matrix(c(-1, -2, 1, 1), 2,2)
# x <- makeCacheMatrix(m)
# x$get()
# inv<-cacheSolve(x)
# inv
# inv<-cacheSolve(x)
# getting chached...
# inv

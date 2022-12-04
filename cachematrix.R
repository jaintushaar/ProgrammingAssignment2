## Put comments here that give an overall description of what your

## functions do
##There are two functions makeCacheMatrix, makecacheMatrix 5 #makeCacheMatrix consists of set, get,setiny, getiny
##library(MASS) is used to calculate inverse for non squared as well as square matrices
install.packages("MASS")
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL #initializing inverse as NULL
  set<-function (y) {
                    x<<-y
                    inv<<-NULL
                    }
  get<-function()x     #function to get matrix x
  setinv<-function (inverse) inv<<-inverse
  getinv<- function(){
                      inver<-ginv(x)
                      inver%*%x #function to obtain inverse of the matrix
                      }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
## write a short comment describing this function
##This is used to get the cache data
cachesolve <- function(x, ...) ##gets cache data
{
   inv<-x$getinv()
   if(!is.null(inv)){        #checking whether inverse is Null 
       message("getting cached data!")
       return(inv)    #returns inverse value
  } 
  data<- x$get()
  inv<-solve(data,...) #calculates inverse value
  x$setinv(inv)
  inv        ## Return a matrix that is the inverse of 'x'
}
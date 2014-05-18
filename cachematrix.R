## These functions create a matrix, obtain its inverse and save it in the cache


## The makeCacheMatrix makes a list of functions for setting a matrix, getting it, 
## setting its inverse and obtaining this inverse

makeCacheMatrix <- function(x = matrix()) {
			
			inv<-NULL
			
			set<-function(y){
					x<<-y
					inv<<-NULL
			}
			
			get<-function() x
			setinverse<-function(solve) inv<<-solve
			getinverse<-function() inv
			
			list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


##  The cacheSolve function takes some elements of the list created in the previous
## function in order to calculate the inverse of the matrix and store it in the cache. 
## But, if it has already been calculated, it does not do it again; it simply retrieves 
## the cached data.

cacheSolve <- function(x, ...) {
			
			inv<-x$getinverse()
			
			if(!is.null(inv)){
					message("getting cached data")
					return(inv)
			}
			
			data<-x$get()
			inv<-solve(data,...)
			x$setinverse(inv)
			inv
	
}

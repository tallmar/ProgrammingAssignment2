## These functions create a matrix, obtain its inverse and save it in the cache


## The makeCacheMatrix function takes a matrix as its argument and produces  
## a list of functions that operate on it, namely  setting a matrix, getting it, 
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


##  The cacheSolve function takes the list created in makeCacheMatrix as and argument and operates with 
## it in order to calculate the inverse of the matrix and store it in the cache. 
## But, if the inverse has already been calculated, it does not do it again; it simply retrieves 
## the cached data. In the end, it returns the inverse of the matrix that we used as an argument for makeCacheMatrix

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

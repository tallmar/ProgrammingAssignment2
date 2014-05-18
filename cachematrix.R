## These functions create a matrix and a list of methods for obtaining its inverse
## 

## This function makes a list of functions for setting a matrix and obtaining its inverse

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


##  This function takes the list created in the previous function and calculates the inverse of the matrix, but if it has already calculated it, it doesn't do it again; it simply retrieves the saved data.

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

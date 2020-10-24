### As usual we give some atributes to our matrix
# like it self and its inverse 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Since we had the assumption of all matrices non-singular
# no extra code for the determinat was requiered. Nevertheless,
# if this happened [det=0] we have added some lines of code
# wich verify and advertice us that the determinat is cero and
# retunrn the 0 matrix.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        data <- x$get()
	  if(det(data)==0){
		message("matrix turned out to be singular: Exiting from the program")
		m<-matrix(rep(nrow(data)*ncol(data),0),nrow(data),ncol(data))
		m
	  } else {
		m <- solve(data, ...)
		x$setinv(m)
		m
	  }

}

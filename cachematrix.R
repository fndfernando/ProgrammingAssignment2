## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## when l==c the value of this positions needs to be 1, else the value must to be 0
## making this on the matrix X the answer is going to the matrix Inverse

solveMatrix <- function(x = matrix()) {
        inversa <- diag(x = 1, nrow=nrow(x), ncol=ncol(x))
        deal <- inversa
        ##cblock is a conter if no have a result in 100 interaction
        cblock<-0
        while(!identical(x,deal)&cblock<100){
                cblock<-cblock+1
                for (c in 1:ncol(x)){
                        for (l in 1:nrow(x)){
                                n<-x[l,c]
                                
                                if (l==c){##diagonal
                                        for (i in 1:nrow(x)){
                                                x[l,i]<-round((x[l,i]/n),digits=5)
                                                inversa[l,i]<-round((inversa[l,i]/n),digits=5)
                                        }
                                                
                                }else if(l<c){##upper
                                        for (i in 1:nrow(x)){
                                                x[l,i]<-round((x[l,i]+(x[l+1,i]*(n*-1))),digits=5)
                                                inversa[l,i]<-round((inversa[l,i]+(inversa[l+1,i]*(n*-1))),digits=5)
                                        }        
                                }else{##below
                                        for (i in 1:nrow(x)){
                                                x[l,i]<-round((x[l,i]+(x[l-1,i]*(n*-1))),digits=5)
                                                inversa[l,i]<-round((inversa[l,i]+(inversa[l-1,i]*(n*-1))),digits=5)
                                        }                                          
                                            
                                }
                        }
                }
        }
        if (cblock==100)
                message("no solution")
        
        inversa
}

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInvMatrix <- function(m=matrix()) inv <<- m
        getInvMatrix <- function() inv
        list(set = set, get = get,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'x
        inv <-x$getInvMatrix()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solveMatrix(data, ...)
        x$setInvMatrix(inv)
        inv
}

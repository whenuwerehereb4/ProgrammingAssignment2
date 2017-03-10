## This script consists of 2 functions work in tandem to: 1) compute
#the inverse of a matrix; and 2) store that result in the cache, so
#that it will not need to be re-computed each time the object needs
#to be accessed (assuming the contents are unchanging).

## This function creates a matrix object (orig_matrix) and stores the
# calculated inverse of that matrix (inverse_matrix)

makeCacheMatrix <- function(orig_matrix = matrix()) {
        inverse_matrix <- NULL
        set <- function(y) {
                orig_matrix <<- y
                inverse_matrix <<- NULL
        }
        get <- function() orig_matrix
        set_CompInverse <- function(CompInverse) inverse_matrix <<- CompInverse
        get_CompInverse <- function() inverse_matrix
        list(set = set, get = get,
             set_CompInverse = set_CompInverse,
             get_CompInverse = get_CompInverse)
}


## This function serves to retrieve the inverse of the cached matrix
# (orig_matrix) that is stored in the makematrix() object's environment

cacheSolve <- function(orig_matrix, ...) {
        ## Return a matrix that is the inverse of 'orig_matrix'
        inverse_matrix <- orig_matrix$get_CompInverse()
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- orig_matrix$get()
        inverse_matrix <- solve(data, ...)
        orig_matrix$set_CompInverse(inverse_matrix)
        inverse_matrix
}

## SOME TEST CODE USED TO CONFIRM MY FUNCTIONS WORKED AFTER RUNNING THE
# ABOVE
## WILL WANT TO REMOVE # ON EACH LINE TO RUN :) 

#m<-matrix(c(2,4,3,1,5,7,3,4,2),nrow=3,ncol=3,byrow=TRUE)
#> a<-makeCacheMatrix(m)
#> a
#$set
#function (y) 
#{
 #       orig_matrix <<- y
  #      inverse_matrix <<- NULL
#}
#<environment: 0x128abb580>
        
#       $get
#function () 
#        orig_matrix
#<environment: 0x128abb580>
        
#        $set_CompInverse
# function (CompInverse) 
#        inverse_matrix <<- CompInverse
#<environment: 0x128abb580>
        
#        $get_CompInverse
#function () 
#        inverse_matrix
#<environment: 0x128abb580>
        
#        > a$get()
#[,1] [,2] [,3]
#[1,]    2    4    3
#[2,]    1    5    7
#[3,]    3    4    2
#> a$get_CompInverse()
#NULL
#> a$set(n)
#> cacheSolve(a)
#[,1]       [,2]
#[1,] -3.906542  0.9719626
#[2,]  6.719626 -1.9158879
#[3,] -2.906542  0.9719626
#[,3]
#[1,]  2.925234
#[2,] -4.775701
#[3,]  1.925234
#> a$get_CompInverse()
#[,1]       [,2]
#[1,] -3.906542  0.9719626
#[2,]  6.719626 -1.9158879
#[3,] -2.906542  0.9719626
#[,3]
#[1,]  2.925234
#[2,] -4.775701
#[3,]  1.925234
#> 
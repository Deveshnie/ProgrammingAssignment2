## Put comments here that give an overall description of what your
## functions do

## This function takes in a matrix and returns that matrix. It is composed of 4 functions, 
## one of which receives a matrix and sets that to be the new matrix (set),
## another which receives the inverse of a matrix (set_inverse_matrix),
## another which returns the matrix provided (get)
## another which returns the inverse of the matrix provided (get_inverse_matrix)
## without any variables declared in the global environment


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #clear m (which is the inverse matrix)
  
  set <- function(y) { #receives matrix
    x <<- y #stores matrix in x
    m <<- NULL #clear m (inverse matrix)
  }
  
  set_inverse_matrix <- function(inverse_matrix) { #receives inverse matrix
    m <<- inverse_matrix #stores inverse matrix in m
  }
  
  get <- function() { #returns matrix received by set() otherwise returns the matrix x entered into the outermost function: makeCacheMatrix
    x 
  }
  
  get_inverse_matrix <- function(){ #returns inverse matrix from set_inverse_matrix() or NULL if no inverse_matrix was received by set_inverse_matrix()
    m
  }
  
  #list of sub-functions available in this function
  list(set = set, 
       get = get,
       set_inverse_matrix = set_inverse_matrix,
       get_inverse_matrix = get_inverse_matrix)
}


## This function takes in makeCacheMatrix and either returns the inverse matrix from this function
## if it exists, otherwise it calculates the inverse of a matrix declared in makeCacheMatrix
## without any variables declared in the global environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse_matrix() # assign the inverse matrix (get_inverse_matrix) obtained from makeCacheMatrix to m
  
  # return inverse matrix "m" if it is not null
  
  if(!is.null(m)) {
    message("getting inverse matrix")
    return(m)
  }
    # if the inverse matrix was not set in makeCacheMatrix then assign the matrix from this function to the variable data
    data <- x$get()
    
    #solve for the inverse of this matrix called data and assign it to m
    m <- solve(data, ...)
    
    #take this inverse matrix m and run it through the set_inverse_matrix function within makeCacheMatrix
    x$set_inverse_matrix(m)
    
    # return this inverse matrix
    m
}


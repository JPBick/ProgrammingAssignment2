## In order to be able to save computing time it may be usefull to cache calculations. 
## The following combination of functions using the fact that R is based on lexical scoping. 
## Objects like the here created makecachematrix are encapseled in an environment (function closures) where local variabels  are stored  as well. 
## Regarding this exercise these are x and m! This means each object i.e. testobj=makecachematrix has its own x and m! Everytime makecachematrix is executed a new environment for the object is created.
## Access to these variables is provided by the functions stored in the list of functions (output of makecachematrix). This is the TRICK!!!



## This function creates a special "matrix" object that can cache its inverse. 
## The output is a list of functions (set, get, setinvmat, getinvmat, details s.b.). 
## The original matrix "x" (numbers as matrix) and the object m is stored in the particular environment where the created "makecachematrix" is encapseled.




makeCacheMatrix <- function(x = matrix()) {              ## As input a matrix is required. X is stored  within the environment of makecachematrix.
        m <- NULL
        set <- function(y) {                             ## Give the user the possibility to manipulate the numbers in the matrix via the set function 
                x <<- y                                  ## and gives m the value Null again to trigger the calculation in cachesolve
                m <<- NULL
        }
        get <- function() x     
                                                         ## A function which outputs the raw matrix in numbers
        
        setinvmat <- function(invmat) m <<-invmat        ## When the function setinv is called by cachesolve this functions stores the calculated inverse matrix as m. 
                                                         ## Note this is quite important. Due to the <<- the object m is stored in the environment where the "makeCacheMatrix" created object is encapseled 
                                                         ## Without <<- the object m would be stored as local variable in setinv
        
        getinvmat <- function() m                        ## This function outputs m (Null or the "previous" calculated invmat)
       
        list(set = set, get = get,                       ## Outputs the lists of functions
             setinvmat = setinvmat,
             getinvmat = getinvmat)
}


## This is a simple function including a If-Else structure. For "makecachematrix" objects which chache'ing a inv matrix the stored one is returned.

cacheSolve <- function(xxx, ...) {                       ## Reads in the object created in makecachmatrix.
        
        mmm <- xxx$getinvmat()                           ## Gives m the stored value NULL or the previous calculated invmat.
                                                         ## It is important to note that the m assigned here differs from the m which is associated to the getinvmat function (function closure).        
        if(!is.null(mmm)) {
                message("getting cached data") 
                return(mmm)                              ## Return the stored matrix that is the inverse of 'x'
        }
        data <- xxx$get()                                ## xxx$get() provides the originally matrix which was stored in the environment of the get() function (in the variable x).
       
        mmm <- solve(data, ...)                          ## assigns the calculated inv matrix to the variable mmm 
       
        xxx$setinvmat(mmm)                               ## Via the function setinvmat() the calculated inverse matrix is stored in environment of setinvmat (<- function(invmat) m <<-invmat).
                                                         ## In this way the calculated inverse matrix is stored in the encaplseld object m (encapseled in the function closure of the makecachematric object) 
       
        
        
        mmm                                              ## Outputs the inverse matrix
}

## In contrast to the notation given in the example, I changed the notation in cacheSolve to distingish between the local variables in makecachmatrix and the ones here. Please Note that the Programm written here
## would as well run fine when replacing xxx=x and mmm=m. In order to show my understanding of the complex scoping rules I prefered to used xxx and mmm.




## The following comandlines could be used to test above routines.
##mattest= matrix(c(1,2,3,17,18,3,5,10,4), 3)
##testobj=makeCacheMatrix(mattest)
##cacheSolve(testobj)
##cacheSolve(testobj)




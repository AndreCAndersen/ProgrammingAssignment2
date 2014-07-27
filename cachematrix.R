## Coursera Course: R-programming, rprog-005, 2014
## Programming Assignment 2: Lexical Scoping
## Student: Andre Christoffer Andersen

## This function creates a matrix "object" which is able to cache the  
## inverse matrix calculation after the first time it is computed. This means 
## that every subsequent inverse call fetches the caches results at constant 
## time, versus the first time which at least uses polynomial time.
## Here, x is the matrix which we want to have a cheched inverse of.
makeCacheMatrix <- function(x = matrix()) {
    inner_matrix = x
    cached_inverse_matrix <- NULL
    
    ## Sets the inner matrix of the matrix wrapper. 
    ## New matrix, means also resetting the cached inverse.
    set <- function(y) {
        ## Notice the <<- operator, which searches through parent environments 
        ## for an existing definition of the variable being assigned.
        inner_matrix <<- y
        cached_inverse_matrix <<- NULL
    }
    
    ## Gets the inner matrix of the matrix wrapper.
    get <- function() {
        inner_matrix
    }
    
    ## Sets the cached inverse matrix of the wrapper.
    setinverse <- function(y) {
        cached_inverse_matrix <<- y
    }
    
    ## Gets the cached inverse matrix of the wrapper.
    getinverse <- function() {
        cached_inverse_matrix
    }
    
    ## Returns the defined wrapper object.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function is a cached wrapper of the base solve function. It is used
## in conjunction with objects created by the makeCachedMatrix function.
cacheSolve <- function(x, ...) {
    wrapper_matrix = x
    
    ## Attempting to fetch the cached inverse matrix of the wrapper matrix.
    inverse_matrix <- wrapper_matrix$getinverse()
    
    ## If the wrapper matrix has a cached version simply return it.
    if(!is.null(inverse_matrix)) {
        return(inverse_matrix)
    }
    
    ## Fetching the inner matrix of the wrapper matrix
    data <- wrapper_matrix$get()
    
    ## Calulating the inverse of the wrapper matrix
    inverse_matrix <- solve(data, ...)
    
    ## Setting the newly calculated inverse of the wrapper matrix.
    x$setinverse(inverse_matrix)
    
    ## Returning the newly calcualted inverse of the wrapper matrix.
    inverse_matrix
}

## This function is used to test if the functions makeCacheMatrix and cacheSolve
## have correct behaviour. It generates the following output:
## > run_tests(1000)
## [1] "Test 1: Speed test"
## [1] "-Success: The cached version (0.0000s) was faster than the uncached 
## version (0.9600s)."
## [1] "Test 2: Equals test results of uncahced version"
## [1] "-Success: The uncached matrix is equal to the expected matrix."
## [1] "Test 3: Equals test results of cahced version"
## [1] "-Success: The cached matrix is equal to the expected matrix."
run_tests <- function (m_size = 1000){
    ## Creating a random matrix
    m <- matrix(runif(m_size^2, 5.0, 7.5), m_size, m_size)
    
    ## Solving the inverse matrix manually
    m_inv_expected = solve(m)
    
    ## Creating wrapped inverse cachable matrix
    m_cached = makeCacheMatrix(m)
    
    ## Time test of uncached version
    start_time <- proc.time()
    
    ## Solving the inverse for the first time uncached.
    m_inv_uncache = cacheSolve(m_cached)
    
    ## Time test results
    time_uncached <- proc.time() - start_time
    time_uncached_output = format(time_uncached['elapsed'], digits=4, nsmall=4)
    
    ## Time test of cached version
    start_time <- proc.time()
    
    ## "Solving" the inverse for the second time, using the cache.
    m_inv_cache = cacheSolve(m_cached)
    
    ## Time test results
    time_cached <- proc.time() - start_time
    time_cached_output = format(time_cached['elapsed'], digits=4, nsmall=4)
    
    ## Printing test results
    print("Test 1: Speed test")
    if(time_uncached['elapsed'] > time_cached['elapsed']){
        print(
            paste(
                "-Success: The cached version (", 
                time_cached_output, 
                "s) was faster than the uncached version (", 
                time_uncached_output, 
                "s).", sep=''
            )
        )
    } else if(time_uncached['elapsed'] < time_cached['elapsed']) {
        print(
            paste(
                "-Failure: The cached version (", 
                time_cached_output, 
                "s) was slower than the uncached version(", 
                time_uncached_output, 
                "s).", sep=''
            )
        )
    } else {
        print(
            paste(
                "-Inconclusive: The cached version (", 
                time_cached_output, 
                "s) was as fast as the uncached version (", 
                time_uncached_output, 
                "s).", sep=''
            )
        )
    }
    
    ## Simple function to check if two matrices are equal in shape and content.
    matequal <- function(x, y){
        is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
    }
    
    print("Test 2: Equals test results of uncahced version")
    if(matequal(m_inv_expected, m_inv_uncache)){ 
        print(
            "-Success: The uncached matrix is equal to the expected matrix."
        )
    } else {
        print(
            "-Failure: The uncached matrix is NOT equal to the expected matrix."
        )
    }
    
    print("Test 3: Equals test results of cahced version")
    if(matequal(m_inv_expected, m_inv_cache)){ 
        print(
            "-Success: The cached matrix is equal to the expected matrix."
        )
    } else {
        print(
            "-Failure: The cached matrix is NOT equal to the expected matrix."
        )
    }
}

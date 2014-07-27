## Created by  : Ashmita Bhattacharjee
## Created on  : 07/27/2014
## Created for : Coursera R Programming : Programming Assignment 2

## This assignment is to write a pair of functions that cache the inverse of a matrix.

## What do we mean by inverse of a matrix ?

## For a square matrix A, the inverse is written A-1. When A is multiplied by A-1 the result is the identity matrix I. 
## Non-square matrices do not have inverses.

## Note: Not all square matrices have inverses. A square matrix which has an inverse is called invertible or nonsingular, 
## and a square matrix without an inverse is called noninvertible or singular.

##
##			AA-1 = A-1A = I
##			

##     Example: 	
##
##	For matrix 	A   =  4  3
##		               3  2   
##
##	inverse is	A-1 = -2  3
##	       		       3 -4
##
##      as AA-1 = 1  0
##		  0  1
##
##  For this assignment, it is assumed that the matrix supplied is always invertible.(As per the assigment)

## Below are the two functions to be created

##
## Funtion 1 makeCacheMatrix : This function creates a special "matrix" object that can cache its inverse.
##

##
## Funtion 2 cacheSolve      : This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
##





## The first function, makeCacheMatrix creates a special "matrix" which is really a list containing function to

##    set       : set the input matrix 
##    get       : get the input matrix
##    setmatrix : set the inverse of the input matrix 
##    getmatrix : get the inverse of the input matrix

## Begin code for makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

## End of code for makeCacheMatrix



## The following function calculates the mean of the special "matrix" created with the above function makeCacheMatrix. 
## However, it first checks to see if the inverse of the matrix  has already been calculated. 
## If so, it gets the inverse of the matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix of the matrix and sets the value of the inverse of the matrix in the cache via the setmatrix function.


## Begin code for cacheSolve

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)    
    m
}

## End of code for cacheSolve

### Introduction

This is second programming assignment where I attempt to create the functions
which attempt to demonstrate the caching of the inverse of a matrix rather than 
computing it repeatedly

I have written following functions:

1.  `makeCacheMatrix`: This function creates a special "matrix" object
    that can cache its inverse.
2.  `cacheSolve`: This function computes the inverse of the special
    "matrix" returned by `makeCacheMatrix` above. If the inverse has
    already been calculated (and the matrix has not changed), then
    `cacheSolve` should retrieve the inverse from the cache.

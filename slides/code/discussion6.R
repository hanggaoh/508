#####################################################################
# EC508
# Brief Review of Matrix Algebra
#####################################################################
rm(list=ls())  # clear all

# Define matrices
# by row
matA <- matrix(c(1,2,3,0,1,5,5,6,0), nrow=3, ncol=3, byrow =TRUE) # 3x3 square matrix
# by column (default)
matB <- matrix(c(1:9), nrow=3, ncol=3) # 3x3 square matrix

# Define vectors
veca <- c(5,4,6)
vecb <- c(1,3,-2)

# Basic Matrix Operations
# element-wise multiplication
matA*matB
# element-wise division
matA/matB
# element-wise addition
matA +3
# matrix * constant
matA*3
# matrix / constant
matA/3
# matrix multiplication
matA %*% matB
# outer product of u and v = uv': 3x3 matrix
veca %o% vecb
# inner product of u and v = u'v : scalar
veca %*% vecb
# A'B
crossprod(matA,matB)
# A'A
crossprod(matA)

# dim of matrix A
dim(matA)
# transpose
t(matA)
# creates a diagonal matrix with elements of vectx as diagonal elts
diag(veca)
# returns the diagonal vector of A
diag(matA)
# combines matrices horizontally
cbind(matA, matB)
# combine matrics vertically
rbind(matA, matB)
# vector of row means
rowMeans(matA)
# vector of row sums
rowSums(matA)
# vector of column means
colMeans(matA)
# vector of column sums
colSums(matA)

# determinant of A
det(matA) # |A| is not equal to 0. i.e., A is invertible.
# inverse of A when A is square
solve(matA)
# solve for Ax=b
solve(matA, vecb)
solve(matA) %*% vecb # same as above.
# solve (a,b). If b is missing, then an identity matrix is taken as b and 
# the function solve() returns the inverse of a.

# Kronecker product: kronecker() or %x%
kronecker(matA,matB)
matA %x% matB

# eigenvalues
eigA <-eigen(matA)

# syntax: matrix(data, nrow, ncol, byrow)
# default is byrow=FALSE means the matrix will be filled column by column

seq1 <- seq(1:6)
mat1 <- matrix(seq1, 2)  # nrow = 2 => ncol = 3
mat1

mat2 <- matrix(seq1, 2, byrow = T)   
mat2

mat3 <- matrix(seq1, ncol = 2)
mat3

mat4 <- matrix(seq1, 3, 2)
mat4

mat5 <- matrix(rnorm(20), 4) # nrow = 4
mat5

# appending v1 to mat5
v1 <- c(1, 1, 2, 2)
mat6 <- cbind(mat5, v1)
mat6

v2 <- c(1:6)
mat7 <- rbind(mat6, v2)
mat7


#removing names of rows and columns
#the first NULL refers to all row names, the second to all column names 
dimnames(mat7) <- list(NULL, NULL)
mat7


# By using the bracket notation, we can select rows, columns or elements in a matrix.
# matrix_name[row#, col#]

mat7[1, 6]
mat7[1,] #entire row
mat7[,6] #entire column

mat8 <- matrix(1:6, 2)
mat9 <- matrix(c(rep(1, 3), rep(2, 3)), 2, byrow = T)

# addition & subtraction: dimensions of the matrices should be the same.
mat8 + mat9
mat8 - mat9

# the below will give us an error message as dimensions of the matrices are not the same.
mat7 + mat8



# Q1

Using the matrix-vector multiplication described in Section 2.3.1, applied to the matrix and vector:
    1    2	3	4
5	6	7	8
9	10	11	12
13	14	15	16
1
2
3
4
apply the Map function to this matrix and vector. Then, identify in the list below, one of the key-value pairs that are output of Map.

(1,13)
(1,9)
(2,8)
(4,48)

mat_q1 <- matrix(seq(1,16), nrow = 4, ncol = 4, byrow = T)
vec_q1 <- seq(1:4)

# 2.3.1 function from the book

# "Each Map task will operate on a chunk of the matrix M> From each matrix element m_{i,j}, it procudes the key-value pair (i, m_{i,j}*v_j). Thus, all terms of the sum that make up the compoment x_i of the matrix-vector product will get the same key, i"

# Mapper, row i corresponds to all values with key i
keys <- NULL
for (i in 1:length(vec_q1)) {
    keys <- rbind(keys,(vec_q1 * mat_q1[i,]))
}
keys

# Reducer 
rowSums(keys)

# Q2

Suppose we use the algorithm of Section 2.3.10 to compute the product of matrices M and N. Let M have x rows and y columns, while N has y rows and z columns. As a function of x, y, and z, express the answers to the following questions

1) The output of the Map function has how many different keys? How many key-value pairs are there with each key? How many key-value pairs are there in all?
2) The input to the Reduce function has how many keys? What is the length of the value (a list) associated with each key?

Then, identify the true statement in the list below.

The output of the Map function has 2y pairs with each key. # correct
The input to the Reduce function has pairs with lists of length 2xz.
The output of the Map function has x+z pairs with each key.
The output of the Map function has 2(x+z) pairs with each key.



##
P = M %*% N, dim(M) = x*y, dim(N) = y*z, dim(P) = x*z

key will be (i,k) where i is a row of M andk is a column of N

# The map function: For each element m_{i,j} of M, produce all the key-value pairs ((i,k), (M,j,m_{i,j})) for k = 1,2,..., up to the number of columns of N. Similarly, for each element n_{j,k} of N, produce all the key-value pairs ((i,k), (N,j,n_{j,k})) for i = 1,2,..., up to the number of rows of M. As before, M and N are really bits to tell which of the two relations a value comes from.

1) Number of keys is x*z
2) Each key has y+y values (basically every column from that row from matrix M, and every row from that column from matrix N)

# The Reduce Function: Each key (i,k) will have an associated list with all the values (M,j,m_{i,j}) and (N,j,n_{j,k}), for all possible values of j. The Reduce function needs to connect the two values on the list that have the same value of j, for each j. An easy way to do this step is to sort by j the values that begin with M and sort by j the values that begin wih N, in separate lists. The jth values on each list must have their third components, m_{i,j} and n_{j,k} extracted and multiplied. Then, these products are summed and the result is paired with (i,k) in the output of the Reduce function.




# Q3

Suppose we use the two-stage algorithm of Section 2.3.9 to compute the product of matrices M and N. Let M have x rows and y columns, while N has y rows and z columns. As a function of x, y, and z, express the answers to the following questions:
    The output of the first Map function has how many different keys? How many key-value pairs are there with each key? How many key-value pairs are there in all?
The output of the first Reduce function has how many keys? What is the length of the value (a list) associated with each key?
The output of the second Map function has how many different keys? How many key-value pairs are there with each key? How many key-value pairs are there in all?
Then, identify the true statement in the list below.

The output of the second Map function has xz different keys.
The output of the second Map function has y(x+z) pairs.
The output of the first Reduce function has pairs with lists of length xyz.
The output of the first Reduce function has x+z pairs.

















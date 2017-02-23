Variable description:


iter
	number of entries in final_unif_list

n1
- sample 1 size

n2
- sample 2 size 

k
- number of nearest neighbors

x1
- x coordinates of sample 1 from uniform distribution 

y1
- y coordinates of sample 1 from uniform distribution

x2
- x coordinates of sample 2 from uniform distribution

y2
- y coordinates of sample 2 from uniform distribution

df1_unif
- data points from sample 1 (using x1,y1)

df2_unif
- data points from sample 2 (using x2,y2)

scatterplot_unif
- scatterplot of points from sample 1 and sample 2

df_unif
- sample 1 and sample 2 combined

distance_mat_unif
- matrix of distance between any two points

mat_unif
- matrix of points and their ordered k nearest neighbors 

bool_mat_unif
- matrix of boolean values indicating 1 if neighbor is from sample sample, 0 if not

counter_mat_unif
- matrix of points with the number of their nearest neighbors

newmat_unif
- matrix of number of pairs of nearest neighbors for different combinations of h and h’

ctr
- counter variable used in for loop for computing newmat_unif

ctr_mat
- counter variable

number_of_pairs_mat_unif
- table indicating number of data points (i,i’) for possible values of (h,h’)

nn_unif
- (k+1)x(k+1)matrix form of number_of_pairs_mat_unif

p_est
- matrix of estimated value of P(NN(i)=h, NN(i’)=h’)




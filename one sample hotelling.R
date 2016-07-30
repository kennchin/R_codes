#example of one-sample hotelling t-squared test
#We are interested in determining if calcium content in three
#different areas: plant calcium, plant available calcium, and
#plant exchangeable calcium in different tulips are 
#significantly different from the given
#mu_theta=(2.85,15.00,6.00).

#Data samples
X_1 = c(2.11,10.1,3.4)
X_2 = c(2.36,35.0,4.1)
X_3 = c(2.13,2.0,1.9)
X_4 = c(2.78,6.0,3.8)
X_5 = c(2.17,2.0,1.7)

#Data Matrix
X = matrix(c(X_1,X_2,X_3,X_4,X_5),nrow=5,ncol=3,byrow=T)

#dimension of data
dimx = dim(X)

#Mu_theta
mu_theta=c(2.85,15.00,6.00)

#Calculate sample mean vector and sample covariance matrix
x_mean = apply(X,2,mean)
s = var(X)

#Calculate inverse of sample covariance matrix
inv_s = solve(s)

#Compute one sample hotelling t squared test
Hotel = dimx[1]*t(as.matrix(x_mean-mu_theta))%*%inv_s%*%as.matrix(x_mean-mu_theta)

#Calculate F value and p-value
f_val = (dimx[1]-dimx[2])*Hotel/(dimx[2]*(dimx[1]-1))
df1 = dimx[2]
df2 = dimx[1]-dimx[2]
pval = 1-pf(f_val,df1,df2)

cat("T-squared=",Hotel)
cat("F-value=",f_val,"df=",df1,df2)
cat("p-value=",pval)


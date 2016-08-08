#set working directory where the data is located.
setwd("/Users/Ken/Desktop")

#column names
col_names = c("ID","LSAT","GPA")

#read file 
data = read.table('lawschl.dat.txt',header=F,col.names=col_names)

#create matrix for predictors
X = as.matrix(data[,2:3])

#calculate correlation matrix
corr_matrix = cor(X)
corr_matrix

#Test Ho:rho=0 vs Ha:rho not equal 0
#sqrt(n-2)*r / sqrt(1-r*r) ~ t(n-2,alpha/2)
p = ncol(X)   #number of predictors
n = nrow(X)   #sample size
r = corr_matrix[1,2]   #correlation for LSAT and GPA
t.test = round(sqrt(n-2)*r / sqrt(1-r*r),digits=4)
p_value = round(1-pt(t.test,n-2),digits=5)

#display results
t.test;p_value

#A (1-a)100% CI for rho
#Method 1:fisher Z-transformation
alpha = 0.10
z = 0.5*log((1+r)/(1-r))
z_lower = z - qnorm(1-alpha/2)/sqrt(n-3)
z_upper = z + qnorm(1-alpha/2)/sqrt(n-3)
r_lower = round((exp(2*z_lower)-1)/(exp(2*z_lower)+1),digits=4)
r_upper = round((exp(2*z_upper)-1)/(exp(2*z_upper)+1),digits=4)

#display results
r_lower;r_upper

#Method 2:Bootstrap estimation
n = nrow(X)
B = 1000
result = rep(0,B)
for(i in 1:B)
{
	boot.sample = sample(n,replace=TRUE)
	result[i] = cor(X[boot.sample,])[2]
	
}
sorted = sort(result)
nl = floor(B*alpha/2)+1
nu = floor(B*(1-alpha/2))+1
r_lower_boot = round(sorted[nl],digit=4)
r_upper_boot = round(sorted[nu],digit=4)

#plot histogram
par(mfrow=c(1,1))
hist(result,nclass=100,density=0.01,main = "Bootstrap correlation for Law School Data")

#Alternative: Bootstrap estimation using built-in
library(boot)

#function that calculates the correlation
corr_function = function(x,i){  cor(x[i,])[2]}

corr_boot = boot(X,corr_function,B)

#confidence interval
boot.ci(corr_boot,conf=c(0.80,0.90,0.95))

#plot histogram
  par(fin=c(5,5),mex=1.5)
  title <- "Bootstrap Correlations for the Law School Data"
  hist(corr_boot$t,nclass=30,main=title,freq=F,density=0.01)
 
 
 
 # Draw non-parametric density
  cor.boot.dns <- density(corr_boot$t,n=200,width=.2)
  lines(cor.boot.dns,lty=3,lwd=3)    

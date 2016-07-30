#2-sample hotelling t squared test
# Example: We are interested in determining if there is
#significant difference in temperature has on 0.2% carbon
#steel. We have the following characteristics or attributes:
#yield point and ultimate strength
#think of two matrices


#set working directory
setwd("/Users/Ken/Desktop")

#read data
colnames = c("temp","yield","strength")
steel <- read.table("steel.txt",header = FALSE,col.names=colnames)

#look at the data
head(steel)

#divide into two population(different temperature)
x1 = steel[steel[,1]=="1",2:p]
x2 = steel[steel[,1]=="2",2:p]

#find dimensions of the data
p = dim(steel)[2]
n1 = dim(x1)[1]
n2 = dim(x2)[1]

#compute sample mean vector and sample covariance matrix for each group
m1 = apply(x1,2,mean)
s1 = var(x1)
m2 = apply(x2,2,mean)
s2 = var(x2)

#Note: 1)Covariance matrices are equal, we can pooled them.
# 	   2)Covariance matrices are not equal, 2 cases: a) large samples, T-squared is approx #Chi-squared distribution with p d.f 
# b) small samples, F distribution with p and n1+n2-p-1 df

#Assume covariance matrices are equal
sp = ((n1-1)*s1+(n2-1)*s2)/(n1+n2-2)
T2 = ((n1*n2)/(n1+n2))*t(m1-m2)%*%solve(sp)%*%(m1-m2)
f_val = (n1+n2-p-1)*T2/(p*(n1+n2-2))
df1 = p-1
df2 = n1+n2-(p-1)-1
pval = 1-pf(f_val,df1,df2)
cat("T-squared=",T2)
cat("F-value=",f_val,"df1=",df1,"df2=",df2,"p-value=",pval)

#Covariance are not equal, (barlett's test to determine this)
T2uneq = t(m1-m2)%*%solve(s1/n1+s2/n2)%*%(m1-m2)

df1 = p
df2b = n1+n2-p-1
f_val2 = (n1+n2-p-1)*T2uneq/(p*(n1+n2-2))
p_val2 = 1-pf(f_val2,df1,df2b)


#alternative
St = s1/n1 + s2/n2
a = s1%*%solve(St)/n1
a_squared = a^2
b = s2%*%solve(St)/n2
b_squared = b^2
term1 = (sum(t(a_squared))+sum(t(a))^2)/n1
term2 = (sum(t(b_squared))+sum(t(b))^2)/n2
v= (p+p^2)/(term1+term2)



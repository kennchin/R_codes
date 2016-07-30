#turtles dataset:
#	column1 is gender where 1 is female and 2 male
#	column2,3,4 are length,height, and width of carapace(shell) respectively.


#set working directory
setwd("/Users/Ken/Desktop")

#Add column names
colnames = c("sex","length","height","width")

#Read data from turtles file and convert to matrix form
turtles = as.matrix(read.table("turtles.txt",sep="",col.names = colnames))

#look at the first observations in our dataset
head(turtles)

#finding dimensions of our dataset, we ignore the first column since it's not a predictor
p1 = dim(turtles)[2]
p = p1 - 1

#check for normality of our predictors
qqnorm(turtles[,2])
qqline(turtles[,2],col=2)
qqnorm(turtles[,3])
qqline(turtles[,3],col=2)
qqnorm(turtles[,4])
qqline(turtles[,4],col=2)

#Normality seems better when we perform a transformation of the predictors, it stabilizes
#the variance and outlier.
qqnorm(log(turtles[,2]))
qqline(log(turtles[,2]),col=2)


#Splitting dataset into male and female turtles
turtles_female = as.matrix(turtles[turtles[,1]=="1",2:p1])
turtles_male = as.matrix(turtles[turtles[,1]=="2",2:p1])

#log transformation of dataset
turtles_female_log = log(turtles_female)
turtles_male_log = log(turtles_male)

#Create principal component from covariance matrix:
#	sdev= standard deviation of the component scores(square root of the eigenvalues)
#	rotation = the coefficient needed to compute the scores (elements of eigenvector)
#	x = a nxp matrix of the scores
turtles_female.pc = prcomp(turtles_female_log)
turtles_male.pc = prcomp(turtles_male_log)

#Finding the proportion of variance explained by each principal component
#method 1
total = sum((turtles_female.pc$sdev)^2)
each = as.matrix(turtles_female.pc$sdev)^2
proportion = each/total
cum_proportion = cumsum(proportion)

#method 2
s = var(turtles_female.pc$x)
prop = diag(s)/sum(diag(s))
cum_prop = cumsum(prop)


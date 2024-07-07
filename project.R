# To import dataset
library(readxl)
Water_Potability <- read_excel("C:\\Users\\Suryavenu\\Desktop\\Rstudio\\Water_Potability.xlsx")
View(Water_Potability)
# To display full dataset
Water_Potability
# To display a particular column
Water_Potability$ph
Water_Potability$Hardness
Water_Potability$Solids
Water_Potability$Chloramines
Water_Potability$Sulfate
Water_Potability$Conductivity
Water_Potability$Organic_carbon
Water_Potability$Trihalomethanes
Water_Potability$Turbidity
Water_Potability$Potability
#summary of dataset
summary(Water_Potability)
summary(Water_Potability$ph)
summary(Water_Potability$Chloramines)
summary(Water_Potability$Conductivity)
summary(Water_Potability$Organic_carbon)
summary(Water_Potability$Turbidity)
#To display mean,median, max, min, standard deviation of ph, hardness, turbidity
mean(Water_Potability$ph)
median(Water_Potability$ph)
min(Water_Potability$ph)
max(Water_Potability$ph)
sd(Water_Potability$ph)
#  To display data of a particular column in sequence
seq(Water_Potability$ph)
seq(Water_Potability$Hardness)
seq(Water_Potability$Turbidity)
# To display first 6 rows of dataset
head(Water_Potability)
# To display first 3 rows of dataset
head(Water_Potability,3)
# To display any subset of this dataset, based on specific condition
# Displaying subset, where portability is 1
subset(Water_Potability,Water_Potability$Potability==1) 
# Displaying subset, where portability is 0
subset(Water_Potability,Water_Potability$Potability==0) 
# Displaying subset, where ph is less than 5
subset(Water_Potability,Water_Potability$ph<5) 
# Moments, Skewness and Kurtosis for a particular column
library(moments)
all.moments(Water_Potability$ph) #moments about origin
all.moments(Water_Potability$ph,central=TRUE) #moments about mean
skewness(Water_Potability$ph)
kurtosis(Water_Potability$ph)
# Fit in Distribution
# Normal Distribution for Potability column
# dnorm gives density
par(mfrow=c(2,2))
mp=mean(Water_Potability$Potability)
sp=sd(Water_Potability$Potability)
x=Water_Potability$Potability
y=dnorm(x,mp,sp)
plot(x,y,type='l',col='yellow',main="Density",xlab="Potability")
xp=seq(25,49)
yp=dnorm(xp,mp,sp)
polygon(c(24,xp,49),c(0,yp,0),col='black')
xnp=seq(0,23)
ynp=dnorm(xnp,mp,sp)
polygon(c(0,xnp,23),c(0,ynp,0),col='skyblue')
# pnorm gives distribution function
mp=mean(Water_Potability$Potability)
sp=sd(Water_Potability$Potability)
x=Water_Potability$Potability
y=pnorm(x,mp,sp)
plot(x,y,type='l',col='black',main="Distribution Function",xlab="Potability")
xp=seq(25,49)
yp=pnorm(xp,mp,sp)
polygon(c(24,xp,49),c(0,yp,0),col='orange')
xnp=seq(0,23)
ynp=pnorm(xnp,mp,sp)
polygon(c(0,xnp,23),c(0,ynp,0),col='skyblue')
# Normal Distribution for ph column
# dnorm gives density
m=mean(Water_Potability$ph)
s=sd(Water_Potability$ph)
x=Water_Potability$ph
xyph=dnorm(x,m,s)
plot(x,yph,type='l',col='yellow',main="Density",xlab="Ph")
x1=seq(25,49)
y1=dnorm(x1,m,s)
polygon(c(24,x1,49),c(0,y1,0),col='red')
x2=seq(0,23)
y2=dnorm(x2,m,s)
polygon(c(0,x2,23),c(0,y2,0),col='skyblue')
# pnorm gives distribution function
m=mean(Water_Potability$ph)
s=sd(Water_Potability$ph)
x=Water_Potability$ph
yph=pnorm(x,m,s)
plot(x,yph,type='l',col='black',main="Distribution Function",xlab="Ph")
x1=seq(25,49)
y12=pnorm(x1,m,s)
polygon(c(24,x1,49),c(0,y12,0),col='orange')
x2=seq(0,23)
y22=pnorm(x2,m,s)
polygon(c(0,x2,23),c(0,y22,0),col='skyblue')

# Testing of Hypothesis
# FOR LARGE SAMPLE
# single mean test
x1 = Water_Potability$ph[30:49] # sample
xbar = mean(x1)
mu0 = mean(Water_Potability$ph) # of population
sigma = sd(x1)
sigmapop = sd(Water_Potability$ph)
n=length(x1)
z=(xbar-mu0)/(sigmapop/sqrt(n))
z
alpha=0.05 # for 5% level of significance
zhalfalpha=qnorm(1-(alpha/2))
if(z< abs(zhalfalpha)){print("Ho is accepted, and sample belongs to population")}else{print("Ho is rejected, and sample does not belong to population")}

#FOR SMALL SAMPLE
# F-test, is ratio of variance or Variance-test
sample1=Water_Potability$Conductivity
sample1
sample2=Water_Potability$Turbidity
sample2
f=var.test(sample1,sample2)
f
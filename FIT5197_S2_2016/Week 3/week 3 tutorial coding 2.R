#######################################################################################
# week 3 tutorial 
# in class examples 
#######################################################################################

########################## example of noise in data ###################################
set.seed(275280)
x <- runif(100, min = 0, max = 1)  
x
mean(x) # 0.5129629
var(x) # 0.09078135

# # add noise to 10% of the entire data
indices <- rbinom(length(x), 1, 0.1) # sample indices, where noise will be added
indices
indices <- as.logical(indices) # convert 0, 1 to FALSE, TRUE
indices
noise <- rnorm(sum(indices), mean = 3, sd = 1) # generate noise from N(3, 1)
noise 
x[indices] <- x[indices] + noise # add noise to corresponding data points

plot(x)

mean(x) # 0.7715149
var(x) # 0.6787696

# increase sample size to reduce the impact of noise
y = c(x, runif(100, min = 0, max = 1))
mean(y) # 0.6394396
var(y) # 0.6394396

# increase sample size, reduce impact of noise on mean
z = c(x, y)
mean(z) # 0.6834647
var(z) # 0.492095


########################## example of missing values ###################################

# sample data
x = runif(100)
y = runif(100)
data = cbind(x, y)
head(data)
index1 = sample(1:100, 30)
index2 = sample(1:100, 30)

# create missing values
data[index1, 1] = NA
data[index2, 2] = NA
head(data)
apply(data, 2, mean, na.rm = TRUE)

# delelte entire rows that contain NAs
data2 = na.omit(data)
dim(data2)

# mean imputation
data3 = data 
data3[is.na(data3[, 1]), 1] = mean(data[, 1], na.rm = TRUE)
data3[is.na(data3[, 2]), 2] = mean(data[, 2], na.rm = TRUE)
apply(data3, 2, mean) # variable means do not change

apply(data, 2, var, na.rm = TRUE)
apply(data3, 2, var) # variable variances are reduced

########################## impact of outliers ########################## 
x = c(4, 4, 5, 5, 5, 5, 6, 6, 6, 7 ,7)
y = c(x, 300)

mean(x) 
mean(y)

median(x)
median(y)

getMode <- function(x) {
  
  uniqueValues <- unique(x)
  uniqueValues[which.max(tabulate(match(x, uniqueValues)))]
  
}

getMode(x)
getMode(y)

sd(x)
sd(y)

########################## example of outliers in data ###################################
# normal probability plot 
# assuming data come from normal distribution
x = c(65, 75, 16, 22, 43, 40, 28, 36, 50, 90)
x = x[order(x)]
x
y = rnorm(10)
y = y[order(y)]
y

plot(x, y)

# R function for normal probability plot 
data = rnorm(100)
data=c(data,17,-90)
qqnorm(data)
qqline(data, col = 2) # add a theoretical line


########################## example of sample size and sd ###################################
# sample size 
par(mfrow = c(2,2))

f = function(sample, stddev) {
  x = rnorm(sample, 0, stddev)
  hist(x, breaks=50, prob=T)
  curve(dnorm(x, mean = 0, sd = stddev), add = T, yaxt = "n")
}

f(100, 1)
f(1000, 1)
f(10000, 1)
f(100000, 1)

f(1000, 0.1)
f(1000, 1)
f(1000, 4)
f(1000, 8)



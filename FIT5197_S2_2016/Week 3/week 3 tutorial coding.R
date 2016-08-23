# 
par(mfrow = c(1, 3))

data = rnorm(100)
boxplot(data, range = 1.5)
y = data

# sd
z = removeOutliers.sd(y)
boxplot(z, range = 1.5)

# 1.5 IQR
z = removeOutliers.IQR(y)
boxplot(z, range = 1.5)

# another data 
y = c(y, 17, -90)
boxplot(y, range = 1.5)

z = removeOutliers.IQR(y)
boxplot(z, range = 1.5)



# 
x1 = runif(100)
x2 = rnorm(100)
x3 = sample(1:5, 100, replace = T)
t = data.frame(x1, x2, x3)

View(t)

hist(t$x1)
hist(t$x2)
hist(t$x3)

seq = seq(0, 1, by = 0.25)

range(x1)
quantile(x1, probs = seq, na.rm = F, names = T)

range(x2)
quantile(x2, probs = seq, na.rm = F, names = T)

range(x3)
quantile(x3, probs = seq, na.rm = F, names = T)

mean(x2)
sd(x2)

tab = table(x3)
tab

prop.table(tab)

barplot(tab)

frequency(x3)

names(t)

summary(t)

describe(t)

hist(x1)
hist(t)
boxplot(t)

# visulization
ineq = read.csv("Week 3/inequality.csv")
par(mfrow=c(2, 2))
hist(ineq$Gini, breaks = 20)
barplot(ineq$Gini, col = rainbow(10))
pie(ineq$Gini, col = rainbow(7), labels = "Gini")
pie(ineq$Gini[1:6], col = rainbow(7), labels = "Gini")

# time series
head(Nile)
mean(Nile)
summary(Nile)
hist(Nile)

plot(Nile, type = "l", xlab = "Years", ylab = "Height of Nile", col = "blue", lwd = 2)

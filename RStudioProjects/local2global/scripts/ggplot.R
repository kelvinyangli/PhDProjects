<<<<<<< HEAD
#require(reshape2)
#library(ggplot2)

#ed = read.csv("../../../UAI_exp/plot_sample.csv")
#ed$Sample = as.factor(ed$Sample)
str(ed)
pd <- position_dodge(width = 0.2)
ggplot(ed, aes(x = Sample, y = Mean, fill = Method)) + 
  geom_point(aes(color = Method, shape = Method), size = 1.5, position = pd) +
  geom_errorbar(aes(ymin = Mean - CI, ymax = Mean + CI),
                width = 0.1, position = pd) + 
  facet_wrap(~Network)
=======
#require(ggplot2)
#require(GGally)
#require(reshape2)
#require(lme4)
#require(compiler)
#require(parallel)
#require(boot)


library(ggplot2)

# create fake dataset with additional attributes - sex, sample, and temperature
x <- data.frame(
  values = c(runif(100, min = -2), runif(100), runif(100, max = 2), runif(100)),
  sex = rep(c('M', 'F'), each = 100),
  sample = rep(c('sample_a', 'sample_b'), each = 200),
  temperature = sample(c('15C', '25C', '30C', '42C'), 400, replace = TRUE)
)

# compare different sample populations across various temperatures
ggplot(x, aes(x = sample, y = values, fill = sex)) +
  geom_boxplot() +
  facet_wrap(~ temperature)


x = c(41.1, 36.4, 37.1, 35.7, 34.8, 29, 16.4, 8.3, 14.6, 7.4, 9.7, 0, 84.5, 83.7, 79.2, 81.1, 69.9, 77.7, 88.5, 108.6, 90.1, 108.8, 90.85, 111.9)
y = rep(c(500, 1000, 5000), 4)
length(x)

m = matrix(x, 12, 2, byrow = TRUE)
m = cbind(c(rep("Insurance", 3), rep("Alarm", 3), rep("Barley", 3), rep("Hailfinder", 3)), y, m)
m[,c(2,3,4)] = as.numeric(m[,c(2,3,4)])
colnames(m) = c("Model", "Sample", "MBPT", "CaMML")


df = rbind(m[,c(1,2,3)],m[,c(1,2,4)])
df=cbind(df, c(rep("MBPT", 12), rep("CaMML", 12)))
colnames(df) = c("Model", "Sample", "editDistance", "Algorithm")
df = data.frame(df)
df = df[,c(3, 4, 2, 1)]
df[,1]=as.numeric(levels(df[,1]))[df[,1]]

df = read.csv("../../../UAI_exp/ijcai.csv")
df[, 2] = as.factor(df[,2])
ggplot(df, aes(x = Sample, y = editDistance)) + geom_boxplot() + facet_wrap(~ Model)
>>>>>>> master

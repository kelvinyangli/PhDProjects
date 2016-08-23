set.seed(275280) # set a seed 
V1 = rbinom(100, 1, 0.5) # p(0) = 0.5, p(1) = 0.5
V2 = rbinom(100, 1, 0.35) # p(0) = 0.65, p(1) = 0.35
V3 = rep(0, 100) # empty vector to store data for variable V3

# sampel data for variable V3 based on values of V1 and V2
for (i in 1:length(V3)) {
  
  if (V1[i] == 0 && V2[i] == 0) {
    
    V3[i] = sample(c(0, 1, 2), 1, replace = FALSE, c(0.2, 0.4, 0.4))
    
  } else if (V1[i] == 0 && V2[i] == 1) {
    
    V3[i] = sample(c(0, 1, 2), 1, replace = FALSE, c(0.6, 0.2, 0.2))
    
  } else if (V1[i] == 1 && V2[i] == 0) {
    
    V3[i] = sample(c(0, 1, 2), 1, replace = FALSE, c(0.46, 0.27, 0.27))
    
  } else if (V1[i] == 1 && V2[i] == 1) {
    
    V3[i] = sample(c(0, 1, 2), 1, replace = FALSE, c(0.06, 0.47, 0.47))
    
  }
  
}

# store all three vectors into a data frame
data = data.frame(V1, V2, V3)

# save data frame into a csv file 
write.csv(data, "file1.csv", row.names = FALSE)





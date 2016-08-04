################################################################################################
#                        Assign1-template.R
################################################################################################
# To use, set your working directory: > setwd("my/local/directory/")
################################################################################################

# read file1.csv into R 
file1 = read.csv("file1.csv", header = TRUE)
  
# view file1 (optional)
View(file1)

# conduct hypothesis test
# null hypothesis: two variables are independent
# alternative hypothesis: two variables are not independent, i.e dependent

# exercise 1a
chisq.test(x = file1$V1, y = file1$V2)
# exercise 1b
chisq.test(x = file1$V1, y = file1$V3)
# exercise 1c
chisq.test(x = file1$V2, y = file1$V3)
# exercise 1d
# combine V1 and V2 into a new variable V12
V12 = rep(0, length(file1$V1))

for (i in 1:length(file1$V1)) {
  
  if (file1$V1[i] == 0 && file1$V2[i] == 1) {
    
    V12[i] = 1
    
  } else if (file1$V1[i] == 1 && file1$V2[i] == 0) {
    
    V12[i] = 2
    
  } else if (file1$V1[i] == 1 && file1$V2[i] == 1) {
    
    V12[i] = 3
    
  }
  
}

# test for independence between the new variable V12 and V3
chisq.test(x = V12, y = file1$V3)

# exercise 2

# exercise 3
file2a = read.csv("file2a.csv")
file2b = read.csv("file2b.csv")

# replace the last value of column 1 by 1 due to there is only one level in the 
# original data set
file2b$V1[nrow(file2b)] = 1 

# exercise 3a
chisq.test(file2b$V1, file2b$V2)
# exercise 3b
chisq.test(file2b$V1, file2b$V10)
# exercise 3c
chisq.test(file2a$V1, file2a$V2)



  
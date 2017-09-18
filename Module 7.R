#MODULE 7

library("sciplot", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

y <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 25, 50, 100, 200, 1000)

#Write a function to determine the geometric mean of the values in a vector. Remember the general form for functions is: 
#function name <- function(arguments to pass){code to run}
#consider adding na.rm=TRUE or na.omit() to functions so they know what to do with NAs

gm1 <- function(x) {
  prod(x)^(1/length(x))}
gm1(y) #13.50559

#Write a function to calculate the sum of squares for a vector
ssl <-function(x) {sum((x-mean(x))^2)}
ssl(y) #917183.3

#Write a function to calculate the population variance for a vector
pop_v<-function(x) {(sum((x-mean(x))^2))/length(x)}
pop_v(y) #61145.56

#Write a function to calculate the sample variance for a vector
sample_v<-function(x) {(sum((x-mean(x))^2))/(length(x)-1)}
sample_v(y) #65513.1

# Write a function to calculate confidence intervals
normalCI = function(x, CIlevel = 0.95) {
  upper = m + qnorm(1 - (1 - CIlevel)/2) * sqrt(var(x)/length(x))
  lower = m + qnorm((1 - CIlevel)/2) * sqrt(var(x)/length(x))
  ci <- c(lower, upper)
  return(ci)
}

z<-c(1, 2, 3,4,5,6,7,8,9,10,11,12,13,14,15)
normalCI(z, CIlevel = 0.95)# function not working 


#can also calculate 95% CI this way by running a simulation:
set <- NULL  # sets up a dummy variable to hold our 10000 simulations
n <- 15
for (i in 1:10000) {
  set[i] <- mean(sample(x, n, replace = TRUE))
}
quantile(set, c(0.025, 0.975))
#RESULT:     2.5%     97.5% 
#        11.93167 236.80000
# I got a different result than the Module

# This function is a Gibbs sampler
##
# Args
# start.a: initial value for a
# start.b: initial value for b
# n.sims: number of iterations to run
# data: observed data, should be in a
# data frame with one column
data=read.table("data-exponential.csv")
##
# Returns:
  # A two column matrix with samples
  # for a in first column and
  # samples for b in second column
sampleGibbs <- function(start.a, start.b, n.sims, data){
  # get sum, which is sufficient statistic
  x <- sum(data)
  # get n
  n <- nrow(data)
  # create empty matrix, allocate memory for efficiency
  res <- matrix(NA, nrow = n.sims, ncol = 2)
  res[1,] <- c(start.a,start.b)
  for (i in 2:n.sims){
    # sample the values
    res[i,1] <- rgamma(1, shape = n+1,
                       rate = res[i-1,2]*x+1)
    res[i,2] <- rgamma(1, shape = n+1,
                       rate = res[i,1]*x+1)
  }
  return(res)
}


# run Gibbs sampler
n.sims <- 10000
# return the result (res)
res <- sampleGibbs(.25,.25,n.sims,data)
head(res)
summary(res[3000:10000,])
hist(res[3000:10000,1])
hist(res[3000:10000,2])


args <- commandArgs(TRUE)
if (length(args) != 4){
  quit()
} else {
  ## parsing command arguments
  for (arg in commandArgs(TRUE)) {
    eval(parse(text = arg))
  }
}
 
## check if a given integer is prime
isPrime = function(n) {
  if (n <= 3) {
    return (TRUE)
  }
  if (any((n %% 2:floor(sqrt(n))) == 0)) {
    return (FALSE)
  }
  return (TRUE)
}
  
## estimate mean only using observation with prime indices
estMeanPrimes = function (x) {
  n = length(x)
  ind = sapply(1:n, isPrime)
  return (mean(x[ind]))
}
  
# step 1: set random seed
set.seed(seed)
    
sum1 <- 0
sum2 <- 0
for ( i in 1:rep) {
  # step 2: generate data according to argument dist
  if (dist == "gaussian"){
    x = rnorm(n, 0, 1)
  }
  else if (dist == "t1"){
    x = rt(n, df = 1)
  }
  else if (dist == "t5"){
    x = rt(n, df = 5)
  }
    
  # estimate mean
  emp <- estMeanPrimes(x)
  emc <- mean(x)
  sum1 <- sum1 + (emp - 0)^2
  sum2 <- sum2 + (emc - 0)^2
}
  
result <- c(sum1/rep, sum2/rep)
#return(result)
result






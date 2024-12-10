set.seed(7900)
ms      <- c(10,30,100)
k       <- 10   
B       <- 30

for (m in ms) {
  train <- matrix(data = 0, ncol = k+1, nrow = m)
  
  
  label <- function(x) {
    return(ifelse(sum(x^2) > qchisq(0.5, df = k), 1, -1))
  }
  invisible(sapply(1:nrow(train), function(j) {
    xs <- rnorm(n = k)
    train[j,] <<- c(xs, label(xs))
  }))
  
  colnames(train) <- c(sapply(1:k, function(k) {paste0("X",k)}),"Y")
  
  bootstrap_data <- function(tr) {
    idxs <- sample(1:nrow(tr), nrow(tr),replace = T)
    return(tr[idxs,])
  }
  boot <- bootstrap_data(train)
  
  res_tot <- 0
  svm_t_size <- 0
  for (b in 1:B) {
    source("./hdp_code.R") 
    res_tot <- res_tot + mean(res)
  }
  
  print(paste0("Estimated Beta (see Arsov): ", res_tot / B))
}
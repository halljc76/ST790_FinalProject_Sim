#########################
#' Simulating the Hastie Dataset
#########################
library(e1071)

# make_imputed   <- function(tr, i = 1) {
#   u <- rnorm(n = k)
#   tr[i,] <- c(u, label(u))
#   # tr[,ncol(tr)] <- sapply(1:nrow(tr), function(j) {label(tr[j,])})
#   tr
# }

cost <- function(x,y) {x != y}

# Z <- boot

for (i in 1:nrow(boot)) {
  temp <- t(as.data.frame(boot[i,]))
  colnames(temp) <- colnames(boot)
  temp[,"Y"] <- -1 * temp[,"Y"]
  Z <- rbind(Z, temp)
}

svm_t <- svm(Y ~ . + .^2, data = boot, type = "C-classification")

res <- sapply(1:m, function(i) {
  svm_i <- svm(Y ~ . + .^2, data = boot[-i,], type = "C-classification")

  temp <- c()
  for (j in 1:nrow(train)) {
    z <- train[j,]
    temp[j] <- abs(cost(
      predict(svm_t, newdata = as.data.frame(t(z))[-ncol(boot)]),
      z[ncol(boot)]
    ) - cost(
      predict(svm_i, newdata = as.data.frame(t(z))[-ncol(boot)]),
      z[ncol(boot)]
    ))
  }
  mean(temp) 
})
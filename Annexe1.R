

eva = function(X, b1, b2){
  res <- c()
  for (i in 1:length(X)) {
    if (X[i] >= b1 & X[i] <= b2) {
      res[i] = 0
    } else if (X[i] >= b1 & X[i] >= b2) {
      res[i] = 1
    } else if (X[i] <= b1 & X[i] <= b2) {
      res[i] = 2
    }
  }
  count = rle(as.character(res))

  res1 = c()
  res2 = c()
  res3 = c()
  res4 = c()
  res5 = c()
  for (i in 1:length(count$lengths)) {
    if (count$values[i] == 1) {
      if (count$lengths[i] <= 10) {
        res1 = c(res1, count$lengths[i])
      } else {
        res2 = c(res2, count$lengths[i])
      }
    } else if (count$values[i] == 2) {
      if (count$lengths[i] <= 10) {
        res3 = c(res3, count$lengths[i])
      } else {
        res4 = c(res4, count$lengths[i])
      }
    } else if (count$values[i] == 0) {
      res5 = c(res5, count$lengths[i])
    }
  }

  # Pourcentage à 90, court
  per1 = (sum(res1)/length(X))*100
  # Pourcentage a 90, long
  per2 = (sum(res2)/length(X))*100
  # Pourcentage a 110, court
  per3 = (sum(res3)/length(X))*100
  # Pourcentage a 110, long
  per4 = (sum(res4)/length(X))*100
  # Pourcentage zone
  per5 = (sum(res5)/length(X))*100

  return(c(per1, per2, per3, per4, per5))
}



library(forecast)

smoothing <- function(data, param=3) {
  len = length(data$secs)-floor(param/2)
  vector_list = data$secs[(1+floor(param/2)):len]
  for (i in 1:dim(data)[2]) {
    if (colnames(data)[i] != "secs") {
      smoothed = na.omit(ma(data[,i], order=param))
      vector_list = cbind(vector_list, smoothed)
    }
  }
  colnames(vector_list) = colnames=colnames(data)
  return(data.frame(vector_list))
}



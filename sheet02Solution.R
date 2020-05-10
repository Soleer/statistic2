# Henning Stein
# Niklas Wünstel

library(ggplot2)
library(reshape2)

# a) ----------------------------------------------------------------

# Solution for exercise a)
vectorcreate <- function(n){
  rt(n, 2)
}

task2 <- function(rep){
  Means <- data.frame(x = 1:200)
  Medians <- data.frame(x = 1:200)
  for(c in 1:rep){
    sampel = vectorcreate(200)
    Means[,c+1] = sapply(1:200, function(n) {
      sum(sampel[1:n])/n
    })
    Medians[,c+1] = sapply(1:200, function(n) {
      median(sampel[1:n])
    })
    
  }
  meltedMeans = melt(Means, id = "x")
  meltedMedians = melt(Medians, id = "x")
  print(ggplot(meltedMeans,aes(x,value)) +geom_line(aes(color = variable)) + ggtitle(" Monte−Carlo simulation for the sample mean"))
  print(ggplot(meltedMedians,aes(x,value)) +geom_line(aes(color = variable)) + ggtitle(" Monte−Carlo simulation for the sample median"))
}

task2(20)
# Henning Stein
# Niklas Wünstel

library(ggplot2)
library(reshape2)

# a) ----------------------------------------------------------------

# Solution for exercise a)
vectorcreate <- function(n){
  rnorm(n, mean=0, sd=0.01)
}

taskA <- function(rep){
  data <- data.frame(x = 1:200)
  for(c in 1:rep){
    sampel = vectorcreate(200)
    data[,c+1] = sapply(1:200, function(n) {
      sum(sampel[1:n])/n
    })
    
  }
  melted = melt(data,id = "x")
  print(ggplot(melted,aes(x,value)) +geom_line(aes(color = variable)))
  data
}

taskA(1)


# b) ----------------------------------------------------------------

# Solution for exercise b)
taskA(20)
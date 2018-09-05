library(bitops)
library(RCurl)
library(XML)
library(RJSONIO)
library(DMwR)
library(spatstat)
library(zoom)

k <- 30
elasticSearchIndex <- "outlier-normal"

#getData
data <- getURL(paste("localhost:9200/", elasticSearchIndex,"/test/_search/?size=1000", sep=""))
content <- fromJSON(data)

#convert timestamp
timeTest <- c()
for(var in content$hits$hits){
  timestamp <- as.numeric(strptime(var$`_source`["time"], "%Y-%m-%dT%H:%M:%S"))
  timeTest <- c(timeTest, timestamp)
}

#distance Computation
distance <- nndist(timeTest, k=k)

#Plotting
lines(PlotDistance[order(PlotDistance$timeTest),], col="blue")
points(data.frame(plotPoints$timeTest, c(11000)))

#computation of normal distribution
dataset <- distance
N <- length(dataset)
mu <- sum(dataset) / N
sigmaSquare <-  sum((dataset[]- mu)^2) / N

result2 <- c();
for(datapoint in dataset){
  probability <- ((1/ sqrt(2*pi*sigmaSquare))* (exp(1) ^(-((datapoint - mu)^2) / (2*sigmaSquare))))
  #Log normal distribution
  #probability <- ((1/(sqrt(sigmarQuadrat)*sqrt(2*pi)))*(1/datapoint)*(exp(1)^(-0.5*((log(datapoint)-müh)/sqrt(sigmarQuadrat)))))
  result2 <- c(result2, probability)                       
}
result2 <- data.frame(dataset, result2)
#plotting of normal distribution + thresholds
plot(result2$dataset, result2$result2, col="red")
lines(data.frame(c(3*sqrt(sigmaSquare)+mu), result2$result2), type="l", col="blue")
lines(data.frame(c(mu - 3*sqrt(sigmaSquare)), result2$result2), type="l", col="blue")
#return entries which exceed the threshold
indicies <- which(result2$dataset > (mu + 3*sqrt(sigmaSquare)))
result2$dataset[indicies]
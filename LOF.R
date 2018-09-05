library(bitops)
library(RCurl)
library(XML)
library(RJSONIO)
library(DMwR)

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
#compute LOF
outlier.scores <- lofactor(timeTest, k=k)
outlierIndex <- order(outlier.scores, decreasing=TRUE)#[1:AnzMaxOutlier]

#combine LOF and log-data into one data frame
time <- c()
data <- c()
user <- c()
lof <- c()
for(datapoint in outlierIndex){
  time <- c(time, content$hits$hits[[datapoint]]$`_source`["time"])
  lof <- c(lof, outlier.scores[datapoint])
}
result <- data.frame(lof, time)

#View result
View(result)

#Plotting
plotPoints <- data.frame(timeTest, outlier.scores)
PlotOutlierScore <- data.frame(timeTest, outlier.scores)
plot(plotPoints, type="p", col="red", xlab="Time", ylab="Local Outlier Factor")
lines(PlotOutlierScore[order(PlotOutlierScore$timeTest),], col="blue")

#plotting of histogram of lof results
hist(PlotOutlierScore$outlier.scores)

#computation of normal distribution
dataset <- outlier.scores
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







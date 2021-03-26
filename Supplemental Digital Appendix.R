knitr::opts_chunk$set(echo = TRUE) # 
knitr::purl("Supplemental Digital Appendix.Rmd", documentation = 0)

library(timeDate)
library(timeSeries)
library(fBasics)
library(fGarch)

N = 100

a = 175 # transparency value of histogram color, out of 255

set.seed(1)
hawk1 = rsnorm(N, mean = 9, sd = 4, xi = 3)
hawk = data.frame(Raw_Hawk = as.vector(round(hawk1)))
hawk$Raw_Hawk_Sorted = sort(hawk$Raw_Hawk)
ymax = 15 # max(hawk$Raw_Hawk)

hawk_hist = hist(hawk$Raw_Hawk_Sorted, breaks = seq(min(hawk$Raw_Hawk)-1,
  max(hawk$Raw_Hawk), by=1),  col=rgb(255,0,0,a, maxColorValue = 255), lwd=2,
  xlim=range(0,50), ylim=c(0,15), main="Hawk Scores - Raw", xlab="Score",
  ylab="Frequency")

set.seed(1)
jane1 = rnorm(N, 19, 7)
jane = data.frame(Raw_Jane = as.vector(round(jane1)))
jane$Raw_Jane_Sorted = sort(jane$Raw_Jane)

jane_hist = hist(jane$Raw_Jane_Sorted, breaks = seq(min(jane$Raw_Jane)-1,
  max(jane$Raw_Jane), by=1), col=rgb(0,0,255,a, maxColorValue = 255), lwd=2, 
  xlim=range(0,50), ylim=c(0, ymax), main="Jane Scores - Raw", xlab="Score", 
  ylab="Frequency")

set.seed(1)
dove1 = rsnorm(N, mean = 40, sd = 4, xi = 0.5)
dove = data.frame(Raw_Dove = as.vector(round(dove1)))
dove$Raw_Dove_Sorted = sort(dove$Raw_Dove)

dove_hist = hist(dove$Raw_Dove_Sorted, breaks = seq(min(dove$Raw_Dove)-1, 
  max(dove$Raw_Dove), by=1), col=rgb(0,255,0,a, maxColorValue = 255), lwd=2,
  xlim=range(0,50), ylim=c(0,ymax), main="Dove Scores - Raw", xlab="Score",
  ylab="Frequency")

mydata = data.frame(App_No = c(1:N),
                    Hawk_raw = hawk$Raw_Hawk_Sorted,
                    Jane_raw = jane$Raw_Jane_Sorted,
                    Dove_raw = dove$Raw_Dove_Sorted
                                          )
head(mydata, n=20)


ymax = max(c(hawk_hist$counts, jane_hist$counts, dove_hist$counts))

hist(hawk$Raw_Hawk_Sorted, breaks = seq(min(hawk$Raw_Hawk)-1, max(hawk$Raw_Hawk), by=1),
  col=rgb(255,0,0,a, maxColorValue = 255), lwd=2, xlim=range(0,50), 
  ylim=range(0,ymax), main=NULL, xlab=NULL, ylab=NULL)
par(new=TRUE)
hist(jane$Raw_Jane_Sorted, breaks = seq(min(jane$Raw_Jane)-1, max(jane$Raw_Jane), by=1),
  col=rgb(0,0,255,a, maxColorValue = 255), lwd=2, xlim=range(0,50),
  ylim=range(0,ymax), main=NULL, xlab=NULL, ylab=NULL)
par(new=TRUE)
hist(dove$Raw_Dove_Sorted, breaks = seq(min(dove$Raw_Dove)-1, max(dove$Raw_Dove), by=1),
  col=rgb(0,255,0,a, maxColorValue = 255), lwd=2, xlim=range(0,50),
  ylim=range(0,ymax), main="Raw Scores", xlab="Score", ylab="Frequency")

scale_factor = N*3
x = seq(0, 40, length.out = 3*N)
all3 = sort(c(hawk$Raw_Hawk, jane$Raw_Jane, dove$Raw_Dove))

y2 = density(all3)
y5 = scale_factor*y2$y
lines(y2$x, y5, col = "black", lwd = 2)

hawk$rank = rank(hawk$Raw_Hawk)
jane$rank = rank(jane$Raw_Jane)
dove$rank = rank(dove$Raw_Dove)

hawk$normal = qnorm(hawk$rank/(1+N))
jane$normal = qnorm(jane$rank/(1+N))
dove$normal = qnorm(dove$rank/(1+N))

nbreak = max(cbind(length(seq(min(hawk$normal), max(hawk$normal), by=0.1)),
                   length(seq(min(jane$normal), max(jane$normal), by=0.1)),
                   length(seq(min(dove$normal), max(dove$normal), by=0.1))))

hawk_hist2 = hist(hawk$normal, breaks = nbreak,
     col=rgb(255,0,0,a, maxColorValue = 255), lwd=2,
     xlim=range(-2.5,2.5), ylim=c(0,ymax), main="Standardized Hawk Scores",
     xlab="Standard Deviations from Mean",
     ylab="Frequency")

jane_hist2 = hist(jane$normal, breaks = nbreak, 
     col=rgb(0,255,0,a, maxColorValue = 255), lwd=2,
     xlim=range(-2.5,2.5), ylim=c(0,ymax), main="Standardized Jane Scores",
     xlab="Standard Deviations from Mean",
     ylab="Frequency")

dove_hist2 = hist(dove$normal, breaks = nbreak,
     col=rgb(0,0,255,a, maxColorValue = 255), lwd=2,
     xlim=range(-2.5,2.5), ylim=c(0,ymax), main="Standardized Dove Scores",
     xlab="Standard Deviations from Mean",
     ylab="Frequency")

hawk_hist2 = hist(hawk$normal, breaks = nbreak,
     col=rgb(255,0,0,a, maxColorValue = 255), lwd=2,
     xlim=range(-2.5,2.5), ylim=c(0,ymax), main="All Standardized Scores",
     xlab="Standard Deviations from Mean",
     ylab="Frequency")

par(new=TRUE)

jane_hist2 = hist(jane$normal, breaks = nbreak, 
     col=rgb(0,255,0,a, maxColorValue = 255), lwd=2,
     xlim=range(-2.5,2.5), ylim=c(0,ymax), main=NULL, xlab=NULL, ylab=NULL)

par(new=TRUE)

dove_hist2 = hist(dove$normal, breaks = nbreak,
     col=rgb(0,0,255,a, maxColorValue = 255), lwd=2,
xlim=range(-2.5,2.5), ylim=c(0,ymax), main=NULL, xlab=NULL, ylab=NULL)

scale_factor2 = N*0.3
all3_2 = sort(c(hawk$normal, jane$normal, dove$normal))
y2 = density(all3_2)
y_scaled2 = scale_factor2*y2$y
lines(y2$x, y_scaled2, col = "black", lwd = 2)

N = 20

set.seed(1)
hawk1 = rsnorm(N, mean = 9, sd = 4, xi = 3)
hawk = data.frame(Raw_Hawk = as.vector(round(hawk1)))
hawk$Raw_Hawk_Sorted = sort(hawk$Raw_Hawk)
hawk_hist = hist(hawk$Raw_Hawk_Sorted, breaks = seq(min(hawk$Raw_Hawk)-1,
  max(hawk$Raw_Hawk)), plot = FALSE)
ymax = max(hawk_hist$counts)

hawk_hist = hist(hawk$Raw_Hawk_Sorted, breaks = seq(min(hawk$Raw_Hawk)-1,
  max(hawk$Raw_Hawk), by=1),  col=rgb(255,0,0,a, maxColorValue = 255), lwd=2,
  xlim=range(0,50), ylim=c(0,ymax), main="Hawk Scores - Raw", xlab="Score",
  ylab="Frequency")
set.seed(1)
jane1 = rnorm(N, 19, 7)
jane = data.frame(Raw_Jane = as.vector(round(jane1)))
jane$Raw_Jane_Sorted = sort(jane$Raw_Jane)

jane_hist = hist(jane$Raw_Jane_Sorted, breaks = seq(min(jane$Raw_Jane)-1,
  max(jane$Raw_Jane), by=1), col=rgb(0,0,255,a, maxColorValue = 255), lwd=2, 
  xlim=range(0,50), ylim=c(0, ymax), main="Jane Scores - Raw", xlab="Score", 
  ylab="Frequency")
set.seed(1)
dove1 = rsnorm(N, mean = 40, sd = 4, xi = 0.5)
dove = data.frame(Raw_Dove = as.vector(round(dove1)))
dove$Raw_Dove_Sorted = sort(dove$Raw_Dove)

dove_hist = hist(dove$Raw_Dove_Sorted, breaks = seq(min(dove$Raw_Dove)-1, 
  max(dove$Raw_Dove), by=1), col=rgb(0,255,0,a, maxColorValue = 255), lwd=2,
  xlim=range(0,50), ylim=c(0,ymax), main="Dove Scores - Raw", xlab="Score",
  ylab="Frequency")
mydata = data.frame(App_No = c(1:N),
                    Hawk_raw = hawk$Raw_Hawk_Sorted,
                    Jane_raw = jane$Raw_Jane_Sorted,
                    Dove_raw = dove$Raw_Dove_Sorted
                                          )
head(mydata, n=20)

ymax = max(c(hawk_hist$counts, jane_hist$counts, dove_hist$counts))

hist(hawk$Raw_Hawk_Sorted, breaks = seq(min(hawk$Raw_Hawk)-1,
                                        max(hawk$Raw_Hawk), by=1),
  col=rgb(255,0,0,a, maxColorValue = 255), lwd=2, xlim=range(0,50), 
  ylim=range(0,ymax), main=NULL, xlab=NULL, ylab=NULL)
par(new=TRUE)
hist(jane$Raw_Jane_Sorted, breaks = seq(min(jane$Raw_Jane)-1,
                                        max(jane$Raw_Jane), by=1),
  col=rgb(0,0,255,a, maxColorValue = 255), lwd=2, xlim=range(0,50),
  ylim=range(0,ymax), main=NULL, xlab=NULL, ylab=NULL)
par(new=TRUE)
hist(dove$Raw_Dove_Sorted, breaks = seq(min(dove$Raw_Dove)-1,
                                        max(dove$Raw_Dove), by=1),
  col=rgb(0,255,0,a, maxColorValue = 255), lwd=2, xlim=range(0,50),
  ylim=range(0,ymax), main="Raw Scores", xlab="Score", ylab="Frequency")

scale_factor = N*3
x = seq(0, 40, length.out = 3*N)
all3 = sort(c(hawk$Raw_Hawk, jane$Raw_Jane, dove$Raw_Dove))

y2 = density(all3)
y5 = scale_factor*y2$y
lines(y2$x, y5, col = "black", lwd = 2)
hawk$rank = rank(hawk$Raw_Hawk)
jane$rank = rank(jane$Raw_Jane)
dove$rank = rank(dove$Raw_Dove)

hawk$normal = qnorm(hawk$rank/(1+N))
jane$normal = qnorm(jane$rank/(1+N))
dove$normal = qnorm(dove$rank/(1+N))

nbreak = max(cbind(length(seq(min(hawk$normal), max(hawk$normal), by=0.1)),
                   length(seq(min(jane$normal), max(jane$normal), by=0.1)),
                   length(seq(min(dove$normal), max(dove$normal), by=0.1))))
hawk_hist2 = hist(hawk$normal, breaks = nbreak,
     col=rgb(255,0,0,a, maxColorValue = 255), lwd=2,
xlim=range(-2.5,2.5), ylim=c(0,ymax), main="Standardized Hawk Scores",
xlab="Standard Deviations from Mean",
ylab="Frequency")

jane_hist2 = hist(jane$normal, breaks = nbreak, 
     col=rgb(0,255,0,a, maxColorValue = 255), lwd=2,
xlim=range(-2.5,2.5), ylim=c(0,ymax), main="Standardized Jane Scores",
xlab="Standard Deviations from Mean",
ylab="Frequency")

dove_hist2 = hist(dove$normal, breaks = nbreak,
     col=rgb(0,0,255,a, maxColorValue = 255), lwd=2,
xlim=range(-2.5,2.5), ylim=c(0,ymax), main="Standardized Dove Scores",
xlab="Standard Deviations from Mean",
ylab="Frequency")

hawk_hist2 = hist(hawk$normal, breaks = nbreak,
     col=rgb(255,0,0,a, maxColorValue = 255), lwd=2,
xlim=range(-2.5,2.5), ylim=c(0,ymax), main="All Standardized Scores",
xlab="Standard Deviations from Mean",
ylab="Frequency")

par(new=TRUE)

jane_hist2 = hist(jane$normal, breaks = nbreak, 
     col=rgb(0,255,0,a, maxColorValue = 255), lwd=2,
xlim=range(-2.5,2.5), ylim=c(0,ymax), main=NULL, xlab=NULL, ylab=NULL)

par(new=TRUE)

dove_hist2 = hist(dove$normal, breaks = nbreak,
     col=rgb(0,0,255,a, maxColorValue = 255), lwd=2,
xlim=range(-2.5,2.5), ylim=c(0,ymax), main=NULL, xlab=NULL, ylab=NULL)

scale_factor2 = N*0.3
all3_2 = sort(c(hawk$normal, jane$normal, dove$normal))
y2 = density(all3_2)
y_scaled2 = scale_factor2*y2$y
lines(y2$x, y_scaled2, col = "black", lwd = 2)

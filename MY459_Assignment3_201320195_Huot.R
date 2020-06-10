
#######################
#
# MY459
# Assignment 3
#
# Philippe Huot - 201320195
#
#######################


###########
#Cleaning
###########

#Clears environment
rm(list=ls())
      
#Clears plots
dev.off()
      
#Clears console
cat("\014")

###########
#Set-up
###########

setwd("C:/Users/Phil/Documents/École/LSE - LT14/MY459")

Data <- read.csv("USBirthData.csv")

#Looking at the data

head(Data)
str(Data)

#Plotting the data

transparentgray <- rgb (0,0,0,0.1)
plot (Data$DayOfYear,Data$BirthRate,col=transparentgray,pch=16)

##########
#Creating a kreg function
##########

weighted.mean <- function (x,w) sum (x*w)/ sum (w)
rmse <- function (fitted,observed) sqrt ( mean ((observed-fitted)^2))

kreg <- function (x,y,bw,kernel = "uniform",fitted.x=x,loo=FALSE){
output <- list (x=x,y=y,bw=bw,kernel=kernel,fitted.x=fitted.x,loo=loo)
class (output) <- 'kreg'

# stop if trying to do leave-one-out with x values other than the observed

if (! identical (fitted.x,x) & loo==TRUE) stop ("Cannot use leave-one-out option unless fitted.x=x")

# specify kernel function

if (kernel == "uniform") kernelf <-
function (xdiff) dunif (xdiff,min=-bw,max=bw)
if (kernel == "normal") kernelf <-
function (xdiff) dnorm (xdiff,mean=0,sd=bw/(2* qnorm (0.75)))

# define function to calculate a single fitted value

fitted.f <- function (fitted.x,loo=NA) {
if ( is.na (loo)) {
weighted.mean (y, kernelf (fitted.x - x))
} else {
weighted.mean (y[-loo], kernelf (fitted.x - x[-loo]))
}
}

# apply fitted value function to each observation and calculate RMSE

output$fitted.y <- mapply (fitted.f, fitted.x, if (loo) 1: length (x) else NA)
if ( identical (fitted.x,x)) output$rmse <- rmse (output$fitted.y,output$y) else output$rmse <- NA
return (output)
}

########
#Question 1
########

#a) The loo command allows to conduct a cross validation test, i.e., to test the 
#out of sample predictability of our model. It does so by fitting the model n times,
#each time leaving one observation (hence LOO). Each time an estimate is made,
#the missing data point is predicted and the square error is calculated. The sum (or mean) of these
#square errors is the cross validation (CV) score. The value of h (kernel bandwith, but could be other parameter)that minimizes the CV
#score is the one that has the best predictive performance.

#b)
#The fitted.x argument finds the fitted x value according to the LOO model. It is necessary to compute the 
#fitted x point for the "left out" observation: recovering the fitted x will allow us to compute the
#square errors (with the fitted x, we can get the corresponding fitted y, which we need to get the square error).

########
#Question 2
########

#Create a kreg for BirthRate on DayofYear

kregDayBirth <- function(x= "DayOfYear",y= "BirthRate",bw=4,kernel = "uniform",fitted.x=x,loo=FALSE){
output <- list (x=x,y=y,bw=bw,kernel=kernel,fitted.x=fitted.x,loo=loo)
class (output) <- 'kreg'

#Stop if trying to do leave-one-out with x values other than the observed

if (! identical (fitted.x,x) & loo==TRUE) stop ("Cannot use leave-one-out option unless fitted.x=x")

#Specify kernel function

if (kernel == "uniform") kernelf <-
function (xdiff) dunif (xdiff,min=-bw,max=bw)
if (kernel == "normal") kernelf <-
function (xdiff) dnorm (xdiff,mean=0,sd=bw/(2* qnorm (0.75)))

#Define function to calculate a single fitted value

fitted.f <- function (fitted.x,loo=NA) {
if ( is.na (loo)) {
weighted.mean (y, kernelf (fitted.x - x))
} else {
weighted.mean (y[-loo], kernelf (fitted.x - x[-loo]))
}
}

#Apply fitted value function to each observation and calculate RMSE

output$fitted.y <- mapply (fitted.f, fitted.x, if (loo) 1: length (x) else NA)
if ( identical (fitted.x,x)) output$rmse <- rmse (output$fitted.y,output$y) else output$rmse <- NA
return (output)
}

#a)
#A uniform kernel regression would compute the fitted y value for the 14th of July
#by taking the average (with equal weigths)of the birthrates for 4 days around 
#July 14 (14th of July, 2 days before, 2 days after, in practice).

#b)
#Given that December 31 is at the end of the year (bound for the x variable), the fitted y value imputed to 
#that date be an average of the last 4 days of the year. This "bound" issue will make the fitted y values for 
#the 31st, 30th and 29th all the same, actually (observe as a flat line at the end of the plot).

#c)
#A better way would be to compute the fitted value near the end (and beggining) of a year in another manner.
#Potential solutions could include using a local linear model, which allows to capture the trends at the "edge"
#of the year, or perhaps splitting the year in different pieces, using smaller bandwidths at the "edges" of the year.
#Loop commands could also be created, telling the function to start back at the other "edge" of the year when it reaches a bound.
#That said, I have no idea how to program this.

########
#Question 3
########

#a)
#Using calendar years as the CV subsets allows to test for the out of sample predictability of our model at the year-level.
#This is useful as we might want to predict day of the year on birth rate trends in upcoming years - this is the unit that's most interesting to us.

#b)

temp.predictions <- rep (NA, length (Data$BirthRate))
cv.scores <- rep (NA,21)
for (bw in 1:21){
for (yr in 1969:1988){
temp.predictions[Data$Year == yr] <- kreg (
Data$DayOfYear[Data$Year != yr],
Data$BirthRate[Data$Year != yr],
bw,
kernel="uniform",
fitted.x=1:365
)$fitted.y
}
cv.scores[bw] <- rmse (temp.predictions,Data$BirthRate)
}

print(cv.scores)
plot(cv.scores, Data$bw)

#With a CV score of 0.08488207 (the lowest), a model with a bandwidth of 3 seems to be the bandwidth to use: according to our LOO strategy,
# it has the best out-of-sample predictability.

#c)
#I don't really understand this question. I guess plotting the data in somw way would help but I don't see how.
#Intuitively, 3 days seems to be the right answer as it it is small enough to capture enough week-to-week
#trends (and perhaps rough within week trends) without compromising the underlying relationship between day of the year and
#birth rates.

########
#Question 4
########


kreg.best <- kreg (
Data$DayOfYear,
Data$BirthRate,
bw=3,
kernel="uniform",
fitted.x=1:365
)

sims <- 200
BootstrapSmooths <- matrix (NA,sims,365)
DayOfYearTemp <- rep (NA,20*365)
BirthRateTemp <- rep (NA,20*365)
for (sim in 1:sims){
boot.years <- sample (1969:1988,20,replace=TRUE)
for (b in 1:20){
DayOfYearTemp[(b-1)*365 + 1:365] <- Data$DayOfYear[Data$Year == boot.years[b]]
BirthRateTemp[(b-1)*365 + 1:365] <- Data$BirthRate[Data$Year == boot.years[b]]
}
BootstrapSmooths[sim,] <- kreg (
DayOfYearTemp,
BirthRateTemp,
bw=3,
kernel="uniform",
fitted.x=1:365
)$fitted.y
}
percentile.ci.bounds <- apply (BootstrapSmooths,2,quantile, c (0.025,0.975))

#a)

plot(kreg.best$fitted.y, , xlab="Day of Year", ylab="Birth Rate", col="white")
lines(kreg.best$fitted.y, col="red",lty=1)

day <- c(1:365)
lines(day, percentile.ci.bounds[1,],col="blue", lty=2)
lines(day, percentile.ci.bounds[2,], col="blue", lty=2)

#b)
#Given the confidence bands we have, I am confident to say that there is a variation
#in birth rates across the calendar year. If there was no seasonality in birth rates in the 
#underlying population, we would observe very large confidence bands, which would mean the trends
#we observe in our sample appear just out of "luck".

#c)

#Resampling days instead of years in the bootstrap code

kreg.best <- kreg (
Data$DayOfYear,
Data$BirthRate,
bw=3,
kernel="uniform",
fitted.x=1:365
)

sims <- 200
BootstrapSmooths <- matrix (NA,sims,365)
DayOfYearTemp <- rep (NA,20*365)
BirthRateTemp <- rep (NA,20*365)
for (sim in 1:sims){
boot.days <- sample (1:365,365,replace=TRUE)
for (b in 1:365){
DayOfYearTemp[(b-1)+ 1:365] <- Data$DayOfYear[Data$Year == boot.days[b]]
BirthRateTemp[(b-1)+ 1:365] <- Data$BirthRate[Data$Year == boot.days[b]]
}
BootstrapSmooths[sim,] <- kreg (
DayOfYearTemp,
BirthRateTemp,
bw=3,
kernel="uniform",
fitted.x=1:365
)$fitted.y
}
percentile.ci.bounds <- apply (BootstrapSmooths,2,quantile, c (0.025,0.975))

#Well, this is not working. I think it has to do with those lines of code:

#DayOfYearTemp[(b-1)+ 1:365] <- Data$DayOfYear[Data$Year == boot.days[b]]
#BirthRateTemp[(b-1)+ 1:365] <- Data$BirthRate[Data$Year == boot.days[b]]

#d) This question rests on being able to solve the previous one.

#e) Same here. It would be helpful to see the picture. 



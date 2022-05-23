##############FINAL PROJECT############
###Q1 DATA COLLECTION AND PRINTING FIRST FIVE AND LAST FIVE LINES##

setwd("C:/Users/mtmyn/Desktop/Fall Semester 2021/Math 5310 Machine Learning/Project")
ozone <- read.csv("ozone-data.csv",header=T)
dim(ozone)
head(ozone,5)
tail(ozone,5)
str(ozone)

#From the above result, the dimension shows 973 data points (Rows) and 14 variables (Columns). 
#While the first 5 and last  5 rows of the data set are shown above.

###Q2 DATA PREPARATION###
###Q2a CHECKING FOR MISSING VARIABLES###
data = ozone
vnames <- colnames(data)
n <- nrow(data)
out <- NULL
for (j in 1:ncol(data)){
  vname <- colnames(data)[j]
  x <- as.vector(data[,j])
  n1 <- sum(is.na(x), na.rm=TRUE)  # NA
  n2 <- sum(x=="NA", na.rm=TRUE) # "NA"
  n3 <- sum(x==" ", na.rm=TRUE)  # missing
  nmiss <- n1 + n2 + n3
  nmiss <- sum(is.na(x))
  ncomplete <- n-nmiss
  out <- rbind(out, c(col.num=j, v.name=vname, mode=mode(x), n.level=length(unique(x)),
                      ncom=ncomplete, nmiss= nmiss, miss.prop=nmiss/n))
}
out <- as.data.frame(out)
row.names(out) <- NULL
out

#The results from the analysis shows that there is no missing values in the data set. 
#All the variable types are continuous with the exception of target variable which is binary (Categorical).

for (j in 1:NCOL(data)){
  print(colnames(data)[j])
  print(table(data[,j], useNA="ifany"))
}

boxplot(ozone$OxidesofNitrogen,ozone$NitrogenDioxide,ozone$SolarRadiation,
        ozone$Std.Dev.WindDirection,names = c("OxideNitrogen","NitrogenDiOx","SolarRadiation","Std_Dev_Wind_Dir")
        ,col=c("blue","red","green","yellow"),main="Distribution of some selected variables")

#From the distribution of the selected features, most of the features contain outliers with the exception of "Std.Dev.WindDirection"and "SolarRadiation".
#See boxplot above for more detail.

##Q2b Exploratory Data Analysis############
ozoneRate<- ifelse(ozone$Ozone==1, "High ozone", "low ozone")
tab=table(ozoneRate,useNA="no")
barplot(tab,names.arg=row.names(tab),col=c("blue","yellow"),ylab="Frequency",
        xlab="Ozone rate",main="Frequncy distribution of the ozone rate")

The Plot above shows that the frequency of a day having a low ozone is higher than the frequency of a day having a high ozone.

##Ozone and Solar Radiation##
boxplot<-boxplot(ozone$SolarRadiation~ozoneRate,xlab="Ozone rate", ylab="Solar 
                 Radiation",main="Box plot of Solar Radiation on Ozone rate",
                 col=c("blue","yellow"))

hist(ozone$SolarRadiation,main="Histogram of Solar Radiation",xlab="Solar Radiation")

library(nortest)
ad.test(ozone$SolarRadiation)
shapiro.test(ozone$SolarRadiation)

wilcox.test(ozone$SolarRadiation~ozoneRate, data=data, alternative = "two.sided",na.action = na.omit)

##Ozone and Nitric Oxide##
boxplot<-boxplot(ozone$NitricOxide~ozoneRate,xlab="Ozone rate", ylab="Nitric 
                 Oxide",main="Box plot of Nitric Oxide on Ozone rate",
                 col=c("blue","yellow"))

hist(ozone$NitricOxide,main="Histogram of Nitric Oxide",xlab="Nitric Oxide")

library(nortest)
ad.test(ozone$NitricOxide)
shapiro.test(ozone$NitricOxide)

wilcox.test(ozone$NitricOxide~ozoneRate, data=ozone, alternative = "two.sided",na.action = na.omit)

##Ozone and NitrogenDiOxide##
wilcox.test(ozone$NitrogenDioxide~ozoneRate, data=ozone, alternative = "two.sided",na.action = na.omit)
hist(ozone$NitrogenDioxide,main="Histogram of NitrogenDioxide",xlab="NitrogenDioxide")

ad.test(ozone$NitrogenDioxide)
shapiro.test(ozone$NitrogenDioxide)
wilcox.test(ozone$NitricOxide~ozoneRate, data=ozone, alternative = "two.sided",na.action = na.omit)

##Ozone on PM10##
boxplot<-boxplot(ozone$PM10~ozoneRate,xlab="Ozone rate", ylab="PM10", main="Box plot of PM10 on Ozone rate",col=c("blue","yellow"))

hist(ozone$PM10,main="Histogram of PM10",xlab="PM10")

library(nortest)
ad.test(ozone$PM10)
shapiro.test(ozone$PM10)

wilcox.test(ozone$PM10~ozoneRate, data=ozone, alternative = "two.sided",na.action = na.omit)


###DATA PARTITIONING##
dat <- as.data.frame(ozone)
set.seed(123)
n <- nrow(dat)
data.split <- sample(x=1:3, size = n, replace =TRUE, prob=c(0.5, 0.25,0.25))
D1 <- dat[data.split == 1, ]
D2<- dat[data.split == 2, ]
D3<- dat[data.split == 3, ]


D1.x <- model.matrix(as.formula(factor(Ozone)~.), data= D1)[, -1]
D2.x <- model.matrix(as.formula(factor(Ozone)~.), data= D2)[, -1]
D3.x <- model.matrix(as.formula(factor(Ozone)~.), data= D3)[, -1]

D1.y <- D1$Ozone
D2.y <- D2$Ozone
D3.y <- D3$Ozone

# MODEL BUILDING

library(mlbench)
library(caret)  
library(glmnet)
Lambda <- seq(0.0001, 0.5, length.out = 200)
L <- length(Lambda)
OUT <- matrix (0, L, 3)
library(glmnet)
for (i in 1:L){
  fit <- glmnet(x=D1.x, y=D1.y, family ="binomial", alpha =1,
                lambda=Lambda[i], standardize=T, thresh = 1e-07, maxit=1000)
  pred <- predict(fit, newx=D2.x, s=Lambda[i], type="response")
  miss.rate <- mean(D2.y != (pred > 0.5))
  mse <- mean((D2.y - pred)^2)
  OUT[i, ] <- c(Lambda[i], miss.rate, mse)
}
head(OUT)

lambda.best <- OUT[which.min(OUT[, 3]), 1]
lambda.best

plot(OUT[, 1], OUT[,3], type = "b", col = "red", ylab = "MSE",xlab="Lambda",main="Plot of tuning parameter(Lambda) vs MSE")

###Final Model###
D1.D2.x=rbind(D1.x,D2.x)
D1.D2.y=c(D1.y,D2.y)
fit.best <- glmnet (x=D1.D2.x, y=D1.D2.y, family ="binomial", alpha=1,  #LASSO
                    lambda = lambda.best, standardize = T, thresh = 1e-07, maxit=1000)
names(fit.best)
fit.best$beta 
oddRatio_nitricOxide= exp(fit.best$beta[1])
oddRatio_OxidesofNitrogen=exp(fit.best$beta[3])
oddRatio_SolarRadiation = exp(fit.best$beta[12])
oddRatio_nitricOxide
oddRatio_OxidesofNitrogen
oddRatio_SolarRadiation

###MODEL EVALUATION###
pred <- predict(fit.best, newx = D3.x, s =lambda.best, type="response")
library(cvAUC)
yobs <- D3.y
AUC <- ci.cvAUC(predictions = pred, labels =yobs, folds=1:NROW(D3),confidence = 0.95)
auc.ci <- round(AUC$ci, digits = 3)
AUC
auc.ci

library(verification)
mod.glm <- verify(obs = yobs, pred = pred)
roc.plot(mod.glm, plot.thres=NULL)
text(x=0.7, y=0.2, paste("Area under ROC = \n", round(AUC$cvAUC, digits = 3),
                         "\n with 95% \nCI (",auc.ci[1], ",", auc.ci[2], ").", sep = " "), col="blue", cex =1.2)

miss.rate <- mean(D3.y != (pred > 0.5))
mse <- mean((D3.y - pred)^2)
miss.rate
mse

library(caret)
pred1 <- ifelse(pred>0.5, 1, 0)
confusionMatrix(factor(pred1), factor(D3.y))

# CONCLUSION
#The Sensitivity or Recall (TP rate) of 0.9051 (90.5%) indicates that the model has a higher % of detecting Low Ozone rate of a particular day. 
#The Specificity(TN rate) of 0.9307 (93.07%) indicates that the model has a higher % of detecting high  Ozone rate of a particular day. 
#Therefore, our fitted Model has an accuracy of 91.6% with respect to performance and a precision of 94.7% which implies that our Model has a low
#FP rate.
#In conclusion we noticed that our fitted model has an accuracy of 91.6% with a low MSE value of 0.06484241 
#and Misclassification rate of 0.084403361 after our logistic model was penalized using lasso regularization.*/


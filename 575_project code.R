fires <- read.csv("forestfires.csv")

# remove irrelevant variables
fires <- fires[,c(1:4, 8:13)]
fires$mo=0
# create recoded numeric month variable
fires$mo[which(fires$month=="jan")] = 1
fires$mo[which(fires$month=="feb")] = 2
fires$mo[which(fires$month=="mar")] = 3
fires$mo[which(fires$month=="apr")] = 4
fires$mo[which(fires$month=="may")] = 5
fires$mo[which(fires$month=="jun")] = 6
fires$mo[which(fires$month=="jul")] = 7
fires$mo[which(fires$month=="aug")] = 8
fires$mo[which(fires$month=="sep")] = 9
fires$mo[which(fires$month=="oct")] = 10
fires$mo[which(fires$month=="nov")] = 11
fires$mo[which(fires$month=="dec")] = 12
fires$mo <- as.numeric(fires$mo)

fires$weekend <- ifelse(fires$day %in% c("sat","sun"), 1, 0)

fires$coords <- paste(fires$X, fires$Y, sep=",")

library(dplyr)
# get total area burned by coordinate pair
coords <- group_by(fires, coords)
coordssum <- summarise(coords, coordsburned = sum(area))

fires2 <- merge(fires, coordssum, by.x = "coords", by.y="coords")

#fires2 <- fires2[sample(nrow(fires2)),]

#commented out to avoid overwriting training and validation datasets 

#firestrain <- fires2[1:258,]
#write.csv(firestrain, "firestrain.csv")
#firesval <- fires2[259:517,]
#write.csv(firesval, "firesval.csv")

firestrain <- read.csv("/Users/jiachenchen/Desktop/PhD\ Coursework/Fall\ 2021/MA575_Linear_Models/Project/forrest-fires-data/Report4/firestrain.csv")
attach(firestrain)
data <- data.frame(ISI, temp, RH, wind, rain, area, mo, coordsburned)
#plot(data$weekend,data$ISI)


library(ggplot2)
library(GGally)
ggpairs(data, lower = list(continuous = wrap("points", alpha = 0.3, size=0.1)))

#lm1 <- lm(ISI ~ temp+wind+as.factor(month %in% c("aug","sep")))
#summary(lm1)

firetrain_use=firestrain[-20,]
firetrain_use$summer=as.numeric(firetrain_use$month %in% c("aug","sep"))


firetrain_use_summer=firetrain_use[which(firetrain_use$summer==1),]
firetrain_use_others=firetrain_use[which(firetrain_use$summer==0),]

#m.mls <- lm(ISI ~ wind+temp,data=firetrain_use_summer)
#summary(m.mls)

ggplot() + 
  geom_point(data=firetrain_use, aes(x=summer*(temp), y=ISI, color = "MLS"), size = 1)

#m.mls <- lm(ISI ~ wind+I(wind^2)+temp+I(temp^2),data=firetrain_use_summer)
#summary(m.mls)

#m.mls <- lm(ISI ~ wind+temp,data=firetrain_use_summer)
#summary(m.mls)


#m.mls <- lm(ISI ~ wind+temp,data=firetrain_use_others)
#summary(m.mls)

#m.mls <- lm(ISI ~ wind+summer*temp+summer*I(temp^2),data=firetrain_use)
#summary(m.mls)

m.mls <- lm(ISI ~ wind+summer*temp+I(summer*temp^2),data=firetrain_use)
summary(m.mls)

#m.mls <- lm(ISI ~ wind+summer*temp,data=firetrain_use)
#summary(m.mls)

#m.mls <- lm(ISI ~ wind*temp+summer,data=firetrain_use)
#summary(m.mls)


#m.mls <- lm(ISI ~temp+ RH+I(RH^2)+wind+summer,data=firetrain_use)
#summary(m.mls)



#m.mls <- lm(ISI ~ weekend+wind+temp+summer,data=firetrain_use)
#summary(m.mls)

#lm3 <- lm(ISI ~ temp+wind+as.factor(month %in% c("aug","sep")),data=firestrain[-20,])
#summary(lm3)

#firetrain_use=firestrain[-20,]

# Diagnostics -----------------------------------------------------------------------------------------------

# Standard Residuals vs ISI
StanResMLS <- rstandard(m.mls)
dataMLS <- data.frame(ISI=firetrain_use$ISI,StanResMLS)

ggplot() + 
  geom_point(data=dataMLS, aes(x=ISI, y=StanResMLS, color = "MLS"), size = 1) +
  geom_hline(yintercept=2,color='blue') + geom_hline(yintercept=-2, color='blue') +
  scale_color_manual(name = element_blank(), labels = c("MLS"), values = c("blue")) +
  labs(y = "Standarized Residual") + ggtitle("Standarized Residuals MLS Plot") 

# Standarized Residuals vs Fitted
Fitted = fitted(m.mls)
dataMLSFitted <- data.frame(Fitted,StanResMLS)

# MLS Stan. Res. vs Fitted plot 
ggplot() +
  geom_point(data=dataMLSFitted, aes(x=Fitted, y=StanResMLS, color = "MLS"), size = 1) +
  geom_hline(yintercept=2,color='blue') + geom_hline(yintercept=-2, color='blue') +
  scale_color_manual(name = element_blank(), labels = c("MLS"), values = c("blue")) +
  labs(y = "Standarized Residual") + labs(x = "Fitted value") + 
  ggtitle("Standarized Residuals MLS Plot (Fitted) ")

# Test of Normality for Standarized Residuals of MLS
p <- ggplot(data.frame(StanResMLS), aes(sample = StanResMLS)) +
  ggtitle("QQ MLS Plot_Synthesized")
p + stat_qq() + stat_qq_line() 

# Histogram of MLS
ggplot(data = data.frame(StanResMLS), aes(x = StanResMLS)) + geom_histogram(bins = 30) +
  ggtitle("Histogram MLS Plot")

# Validation ------------------------------------------------------------------------------------------------

# Residuals for training data
ResMLS <- resid(m.mls)

firesvalidate=read.csv("/Users/jiachenchen/Desktop/PhD\ Coursework/Fall\ 2021/MA575_Linear_Models/Project/forrest-fires-data/Report4/firesval.csv",header=TRUE)
#train_index=firestrain[,1]
#firesvalidate=fires2[-train_index,]
firesvalidate$summer=as.numeric(firesvalidate$month %in% c("aug","sep"))
# Prediction for validation data
output<-predict(m.mls, se.fit = TRUE, newdata=data.frame(temp=firesvalidate$temp, wind=firesvalidate$wind,summer=firesvalidate$summer))
ResMLSValidation <- firesvalidate$ISI - output$fit

# Mean Square Error for training data
mean((ResMLS)^2)

# Mean Square Error for validation data
mean((ResMLSValidation)^2)

# Relative Mean Square Error for validation data
mean((ResMLSValidation)^2) / mean((firesvalidate$ISI)^2)
#87% not bad

# Create data frame with validation observation and prediction
test = data.frame(firesvalidate$ISI,output$fit, 1:length(output$fit));
colnames(test)[1] = "ISI"
colnames(test)[2] = "Prediction"
colnames(test)[3] = "Index"

# Plot ISI vs Prediction for Validation Data Set 
ggplot(data = test, aes(x = ISI, y = Prediction)) + geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Validation ISI vs Prediction")

# Further comparisons
ggplot(data = test, aes(x = Index)) +
  geom_line(aes(y = ISI, color = "ISI")) + 
  geom_line(aes(y = Prediction, color="Prediction"), linetype="twodash") +  
  scale_color_manual(name = element_blank(), labels = c("ISI","Prediction"),
                     values = c("darkred", "steelblue")) + labs(y = "") + 
  ggtitle("Validation")

# Hard to see, let's zoom in
test2 = test[150:200,]

# Plot ISI vs Prediction for Validation Data Set 
ggplot(data = test2, aes(x = Index)) +
  geom_line(aes(y = ISI, color = "ISI")) + 
  geom_line(aes(y = Prediction, color="Prediction"), linetype="twodash") +  
  scale_color_manual(name = element_blank(), labels = c("ISI","Prediction"),
                     values = c("darkred", "steelblue")) + labs(y = "") +
  ggtitle("Validation")





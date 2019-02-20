#Zeeshan Latifi
#Predict 411 Winter 2018

#Read File in from your working directory
#wine = read.csv("/Users/Zeeshan/Desktop/PREDICT 411/Unit 3/HW 3/Wine_Training.csv")  # read csv file
setwd("/Users/Zeeshan/Desktop/PREDICT 411/Unit 3/HW 3/")
wine <- read.csv("Wine_Training.csv")

#call libraries
library(ggplot2) # For graphical tools
library(MASS) # For some advanced statistics
library(pscl) # For "counting" models (e.g., Poisson and Negative Binomial)
library(dplyr) # For general needs and functions
library(readr)
library(corrplot)

# Note, some of these libraries are not needed for this template code.
library(zoo)
library(psych)
library(ROCR)
library(car)
library(InformationValue)
library(rJava)
library(pbkrtest)
library(car)
library(leaps)
library(glm2)
library(aod)

#take a look at the high level characteristics of the wine data
summary(wine)
str(wine)

#examine the target variable
ggplot(data=wine, aes(wine$TARGET)) + 
  geom_histogram(binwidth =1, 
                 col="BLUE", 
                 aes(fill=..count..))+
  scale_fill_gradient("Count", low = "blue", high = "red")

wine_clean <- na.omit(wine)
cor(wine_clean[sapply(wine_clean, is.numeric)])

######################################### Data Exploration ######################################################
par(mfrow=c(2,2))
hist(wine$AcidIndex, col = "red", xlab = "Acid Index", main = "AcidIndex Hist")
hist(wine$Alcohol, col = "green", xlab = "Alcohol Content", main = "Alcohol Hist")
boxplot(wine$AcidIndex, col = "red", main = "AcidIndex BoxPlot")
boxplot((wine$Alcohol), col = "green", main = "Alcohol Boxplot")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(wine$Chlorides, col = "red", xlab = "Chlorides Index", main = "Chlorides Hist")
hist(wine$CitricAcid, col = "green", xlab = "Citric Acid", main = "CitricAcid Hist")
boxplot(wine$Chlorides, col = "red", main = "Chlorides BoxPlot")
boxplot((wine$CitricAcid), col = "green", main = "CitricAcid Boxplot")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(wine$Density, col = "red", xlab = "Density", main = "Density Hist")
hist(wine$FixedAcidity, col = "green", xlab = "Fixed Acidity", main = "FixedAcidity Hist")
boxplot(wine$Density, col = "red", main = "Density BoxPlot")
boxplot((wine$FixedAcidity), col = "green", main = "FixedAcidity Boxplot")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(wine$LabelAppeal, col = "red", xlab = "Label Appeal", main = "LabelAppeal Hist")
hist(wine$STARS, col = "green", xlab = "Stars", main = "STARS Hist")
boxplot(wine$LabelAppeal, col = "red", main = "LabelAppeal BoxPlot")
boxplot((wine$STARS), col = "green", main = "STARS Boxplot")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(wine$VolatileAcidity, col = "red", xlab = "Volatile Acidity", main = "VolatileAcidity Hist")
hist(wine$pH, col = "green", xlab = "pH", main = "pH Hist")
boxplot(wine$VolatileAcidity, col = "red", main = "VolatileAcidity")
boxplot((wine$pH), col = "green", main = "pH Boxplot")
par(mfrow=c(1,1))

#creat IMP versions of each independent variable
wine$FixedAcidity_IMP <- wine$FixedAcidity
wine$VolatileAcidity_IMP <- wine$VolatileAcidity
wine$CitricAcid_IMP <- wine$CitricAcid
wine$ResidualSugar_IMP <- wine$ResidualSugar
wine$Chlorides_IMP <- wine$Chlorides
wine$FreeSulfurDioxide_IMP <- wine$FreeSulfurDioxide
wine$TotalSulfurDioxide_IMP <- wine$TotalSulfurDioxide
wine$Density_IMP <- wine$Density
wine$pH_IMP <- wine$pH
wine$Sulphates_IMP <- wine$Sulphates
wine$Alcohol_IMP <- wine$Alcohol
wine$LabelAppeal_IMP <- wine$LabelAppeal
wine$AcidIndex_IMP <- wine$AcidIndex
wine$STARS_IMP <- wine$STARS

#replace NA's in each column with mean
wine$FixedAcidity_IMP[which(is.na(wine$FixedAcidity_IMP))] <- mean(wine$FixedAcidity_IMP,na.rm = TRUE)
wine$VolatileAcidity_IMP[which(is.na(wine$VolatileAcidity_IMP))] <- mean(wine$VolatileAcidity_IMP,na.rm = TRUE)
wine$CitricAcid_IMP[which(is.na(wine$CitricAcid_IMP))] <- mean(wine$CitricAcid_IMP,na.rm = TRUE)
wine$ResidualSugar_IMP[which(is.na(wine$ResidualSugar_IMP))] <- mean(wine$ResidualSugar_IMP,na.rm = TRUE)
wine$Chlorides_IMP[which(is.na(wine$Chlorides_IMP))] <- mean(wine$Chlorides_IMP,na.rm = TRUE)
wine$FreeSulfurDioxide_IMP[which(is.na(wine$FreeSulfurDioxide_IMP))] <- mean(wine$FreeSulfurDioxide_IMP,na.rm = TRUE)
wine$TotalSulfurDioxide_IMP[which(is.na(wine$TotalSulfurDioxide_IMP))] <- mean(wine$TotalSulfurDioxide_IMP,na.rm = TRUE)
wine$Density_IMP[which(is.na(wine$Density_IMP))] <- mean(wine$Density_IMP,na.rm = TRUE)
wine$pH_IMP[which(is.na(wine$pH_IMP))] <- mean(wine$pH_IMP,na.rm = TRUE)
wine$Sulphates_IMP[which(is.na(wine$Sulphates_IMP))] <- mean(wine$Sulphates_IMP,na.rm = TRUE)
wine$Alcohol_IMP[which(is.na(wine$Alcohol_IMP))] <- mean(wine$Alcohol_IMP,na.rm = TRUE)
wine$LabelAppeal_IMP[which(is.na(wine$LabelAppeal_IMP))] <- mean(wine$LabelAppeal_IMP,na.rm = TRUE)
wine$AcidIndex_IMP[which(is.na(wine$AcidIndex_IMP))] <- mean(wine$AcidIndex_IMP,na.rm = TRUE)
wine$STARS_IMP[which(is.na(wine$STARS_IMP))] <- mean(wine$STARS_IMP,na.rm = TRUE)

#flag NA values with new field
#first, create new field
#second, replace NA's with 1 else 0

wine$ResidualSugar_IMP_Flag <- wine$ResidualSugar
wine$Chlorides_IMP_Flag <- wine$Chlorides
wine$FreeSulfurDioxide_IMP_Flag <- wine$FreeSulfurDioxide
wine$TotalSulfurDioxide_IMP_Flag <- wine$TotalSulfurDioxide
wine$pH_IMP_Flag <- wine$pH
wine$Sulphates_IMP_Flag <- wine$Sulphates
wine$Alcohol_IMP_Flag <- wine$Alcohol
wine$STARS_IMP_Flag <- wine$STARS


#NEW BINARY FIELDS TO SHOW na's
wine$ResidualSugar_IMP_Flag <- ifelse(is.na(wine$ResidualSugar_IMP_Flag)==TRUE, 1,0) 
wine$Chlorides_IMP_Flag <- ifelse(is.na(wine$Chlorides_IMP_Flag)==TRUE, 1,0)
wine$FreeSulfurDioxide_IMP_Flag <- ifelse(is.na(wine$FreeSulfurDioxide_IMP_Flag)==TRUE, 1,0)
wine$TotalSulfurDioxide_IMP_Flag <- ifelse(is.na(wine$TotalSulfurDioxide_IMP_Flag)==TRUE, 1,0)
wine$pH_IMP_Flag <- ifelse(is.na(wine$pH_IMP_Flag)==TRUE, 1,0)
wine$Sulphates_IMP_Flag <- ifelse(is.na(wine$Sulphates_IMP_Flag)==TRUE, 1,0)
wine$Alcohol_IMP_Flag <- ifelse(is.na(wine$Alcohol_IMP_Flag)==TRUE, 1,0)
wine$STARS_IMP_Flag <- ifelse(is.na(wine$STARS_IMP_Flag)==TRUE, 1,0) #LOOK FOR MISSING STAR OBSERVATIONS

summary(wine)


#Is it possible to distinguish red vs white wines by the chemical property makeup?
plot(wine$VolatileAcidity_IMP)

#A better way to visualize volatile acidity
ggplot(data=wine, aes(wine$VolatileAcidity_IMP)) + 
  geom_histogram(binwidth =1, 
                 col="BLUE", 
                 aes(fill=..count..))+
  scale_fill_gradient("Count", low = "blue", high = "red")

summary(wine$VolatileAcidity_IMP)

#make new indicator that indicates red vs white based on volatile acidity
wine$VolatileAcidity_IMP_REDFLAG <- ifelse(wine$VolatileAcidity_IMP > mean(wine$VolatileAcidity_IMP),1,0)
wine$ResidualSugar_IMP_REDFLAG <- ifelse(wine$ResidualSugar_IMP < mean(wine$ResidualSugar_IMP),1,0)
wine$TotalSulfurDioxide_IMP_REDFLAG <- ifelse(wine$TotalSulfurDioxide_IMP < mean(wine$TotalSulfurDioxide_IMP),1,0)
wine$Density_IMP_REDFLAG <- ifelse(wine$Density_IMP > mean(wine$Density_IMP),1,0)
wine$TallyUp <- wine$VolatileAcidity_IMP_REDFLAG + wine$ResidualSugar_IMP_REDFLAG + wine$TotalSulfurDioxide_IMP_REDFLAG + wine$Density_IMP_REDFLAG
wine$Final_REDFLAG <- ifelse(wine$TallyUp > mean(wine$TallyUp),1,0)

pairs(wine[,c("Final_REDFLAG","VolatileAcidity_IMP")])

plot( wine$VolatileAcidity_IMP,wine$TARGET)

#Add Target Flag for 0 sale scenarios
wine$TARGET_Flag <- ifelse(wine$TARGET >0,1,0)
wine$TARGET_AMT <- wine$TARGET - 1
wine$TARGET_AMT <- ifelse(wine$TARGET_Flag == 0,NA,wine$TARGET-1)

cor_wine <- subset(wine, select = c(TARGET, AcidIndex_IMP, Alcohol_IMP, Chlorides_IMP, CitricAcid_IMP, Density_IMP,
                                    FixedAcidity_IMP,FreeSulfurDioxide_IMP, LabelAppeal_IMP, ResidualSugar_IMP,
                                    STARS_IMP, Sulphates_IMP,TotalSulfurDioxide_IMP, VolatileAcidity_IMP, 
                                    pH_IMP), na.rm = TRUE)
c <- cor(cor_wine)
corrplot(c, method = "square")



############################################ Variable Selection ##################################################
reg1 <- regsubsets(TARGET ~  AcidIndex_IMP + Alcohol_IMP + Chlorides_IMP + CitricAcid_IMP + Density_IMP + 
                     FixedAcidity_IMP + FreeSulfurDioxide_IMP + LabelAppeal_IMP + ResidualSugar_IMP + STARS_IMP +
                     Sulphates_IMP + TotalSulfurDioxide_IMP + VolatileAcidity_IMP + pH_IMP + Alcohol_IMP_Flag +
                     Chlorides_IMP_Flag + Density_IMP_REDFLAG + FreeSulfurDioxide_IMP_Flag + ResidualSugar_IMP_Flag +
                     ResidualSugar_IMP_REDFLAG + STARS_IMP_Flag + Sulphates_IMP_Flag + TotalSulfurDioxide_IMP_Flag +
                     TotalSulfurDioxide_IMP_REDFLAG + VolatileAcidity_IMP_REDFLAG + 
                     pH_IMP_Flag, data = wine_impute, nvmax = 50)

summary(reg1)


#######################################################
#######################################################
## FIRST MODEL ... REGULAR LINEAR REGRESSION MODEL#####

##########################################################################################
##########################################################################################
## SECOND MODEL ... REGULAR LINEAR REGRESSION MODEL USING STEPWISE VARIABLE SELECTION (AIC)
##########################################################################################

stepwise_lm <- stepAIC(lm_fit, direction="both")
stepwise_lm$anova


lm_fit_stepwise <- lm(TARGET~ AcidIndex_IMP + Alcohol_IMP + Density_IMP + LabelAppeal_IMP + STARS_IMP + 
                        Density_IMP_REDFLAG + ResidualSugar_IMP_REDFLAG + STARS_IMP_Flag + 
                        TotalSulfurDioxide_IMP_REDFLAG + VolatileAcidity_IMP_REDFLAG, data=wine)

summary(lm_fit_stepwise)
coefficients(lm_fit_stepwise)
wine$fittedLMStepwise <-fitted(lm_fit_stepwise)
AIC(lm_fit_stepwise)

##########################################################################################
##########################################################################################
## THIRD MODEL ... POISSON################################################################
##########################################################################################

poisson_model <- glm(TARGET ~ AcidIndex_IMP
                     +Alcohol_IMP
                     +Density_IMP
                     +LabelAppeal_IMP
                     +STARS_IMP
                     +Density_IMP_REDFLAG
                     +ResidualSugar_IMP_REDFLAG
                     +STARS_IMP_Flag
                     +TotalSulfurDioxide_IMP_REDFLAG
                     +VolatileAcidity_IMP_REDFLAG, family="poisson"(link="log"), data=wine)

summary(poisson_model)
coef(poisson_model)

wine$poisson_yhat <- predict(poisson_model, newdata = wine, type = "response")


##########################################################################################
##########################################################################################
## FOURTH MODEL ... NEGATIVE BINOMIAL DISTRIBUTION########################################
##########################################################################################

NBR_Model<-glm.nb(TARGET ~ 
                    +AcidIndex_IMP
                  +Alcohol_IMP
                  +Density_IMP
                  +LabelAppeal_IMP
                  +STARS_IMP
                  +Density_IMP_REDFLAG
                  +ResidualSugar_IMP_REDFLAG
                  +STARS_IMP_Flag
                  +TotalSulfurDioxide_IMP_REDFLAG
                  +VolatileAcidity_IMP_REDFLAG, data=wine)

summary(NBR_Model)

wine$NBRphat <- predict(NBR_Model, newdata = wine, type = "response")



##########################################################################################
##########################################################################################
## FIFTH MODEL ... ZERO INFLATED POISSON (ZIP)############################################
##########################################################################################

ZIP_Model<-zeroinfl(TARGET ~ AcidIndex_IMP
                    +Alcohol_IMP
                    +Density_IMP
                    +LabelAppeal_IMP
                    +STARS_IMP
                    +Density_IMP_REDFLAG
                    +ResidualSugar_IMP_REDFLAG
                    +STARS_IMP_Flag
                    +TotalSulfurDioxide_IMP_REDFLAG
                    +VolatileAcidity_IMP_REDFLAG, data=wine)

summary(ZIP_Model)

wine$ZIPphat <- predict(ZIP_Model, newdata = wine, type = "response")
AIC(ZIP_Model)

##########################################################################################
##########################################################################################
## 6TH MODEL ... ZERO INFLATED NEGATIVE BINOMIAL REGRESSION (ZINB)########################
##########################################################################################

ZINB_Model<-zeroinfl(TARGET ~ AcidIndex_IMP
                     +Alcohol_IMP
                     +Density_IMP
                     +LabelAppeal_IMP
                     +STARS_IMP
                     +Density_IMP_REDFLAG
                     +ResidualSugar_IMP_REDFLAG
                     +STARS_IMP_Flag
                     +TotalSulfurDioxide_IMP_REDFLAG
                     +VolatileAcidity_IMP_REDFLAG, dist = "negbin", EM=TRUE)
summary(ZINB_Model)

wine$ZINBphat <- predict(ZINB_Model, newdata = wine, type = "response")
AIC(ZINB_Model)

#what type of dispersion does sample have?
mean(wine$TARGET)
var(wine$TARGET)



###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
# Everything below could be a "stand alone" scoring program, in its separate file

#Read File in from your working directory
#wine = read.csv("WINE.csv")  # read csv file
wine_test = read.csv("WINE_TEST.csv")  # read csv file

#call libraries
library(ggplot2)
library(MASS)
library(pscl)
library(dplyr)

wine_test$INDEX <- as.factor(wine_test$INDEX)
win_test$TARGET <- as.factor(wine_test$TARGET)

#creat IMP versions of each independent variable (wine)
wine$FixedAcidity_IMP <- wine$FixedAcidity
wine$VolatileAcidity_IMP <- wine$VolatileAcidity
wine$CitricAcid_IMP <- wine$CitricAcid
wine$ResidualSugar_IMP <- wine$ResidualSugar
wine$Chlorides_IMP <- wine$Chlorides
wine$FreeSulfurDioxide_IMP <- wine$FreeSulfurDioxide
wine$TotalSulfurDioxide_IMP <- wine$TotalSulfurDioxide
wine$Density_IMP <- wine$Density
wine$pH_IMP <- wine$pH
wine$Sulphates_IMP <- wine$Sulphates
wine$Alcohol_IMP <- wine$Alcohol
wine$LabelAppeal_IMP <- wine$LabelAppeal
wine$AcidIndex_IMP <- wine$AcidIndex
wine$STARS_IMP <- wine$STARS

#creat IMP versions of each independent variable (wine_test)
wine_test$FixedAcidity_IMP <- wine_test$FixedAcidity
wine_test$VolatileAcidity_IMP <- wine_test$VolatileAcidity
wine_test$CitricAcid_IMP <- wine_test$CitricAcid
wine_test$ResidualSugar_IMP <- wine_test$ResidualSugar
wine_test$Chlorides_IMP <- wine_test$Chlorides
wine_test$FreeSulfurDioxide_IMP <- wine_test$FreeSulfurDioxide
wine_test$TotalSulfurDioxide_IMP <- wine_test$TotalSulfurDioxide
wine_test$Density_IMP <- wine_test$Density
wine_test$pH_IMP <- wine_test$pH
wine_test$Sulphates_IMP <- wine_test$Sulphates
wine_test$Alcohol_IMP <- wine_test$Alcohol
wine_test$LabelAppeal_IMP <- wine_test$LabelAppeal
wine_test$AcidIndex_IMP <- wine_test$AcidIndex
wine_test$STARS_IMP <- wine_test$STARS

#replace NA's in each column with median since regression approach is not showing any value (wine)
wine$FixedAcidity_IMP[which(is.na(wine$FixedAcidity_IMP))] <- mean(wine$FixedAcidity_IMP,na.rm = TRUE)
wine$VolatileAcidity_IMP[which(is.na(wine$VolatileAcidity_IMP))] <- mean(wine$VolatileAcidity_IMP,na.rm = TRUE)
wine$CitricAcid_IMP[which(is.na(wine$CitricAcid_IMP))] <- mean(wine$CitricAcid_IMP,na.rm = TRUE)
wine$ResidualSugar_IMP[which(is.na(wine$ResidualSugar_IMP))] <- mean(wine$ResidualSugar_IMP,na.rm = TRUE)
wine$Chlorides_IMP[which(is.na(wine$Chlorides_IMP))] <- mean(wine$Chlorides_IMP,na.rm = TRUE)
wine$FreeSulfurDioxide_IMP[which(is.na(wine$FreeSulfurDioxide_IMP))] <- mean(wine$FreeSulfurDioxide_IMP,na.rm = TRUE)
wine$TotalSulfurDioxide_IMP[which(is.na(wine$TotalSulfurDioxide_IMP))] <- mean(wine$TotalSulfurDioxide_IMP,na.rm = TRUE)
wine$Density_IMP[which(is.na(wine$Density_IMP))] <- mean(wine$Density_IMP,na.rm = TRUE)
wine$pH_IMP[which(is.na(wine$pH_IMP))] <- mean(wine$pH_IMP,na.rm = TRUE)
wine$Sulphates_IMP[which(is.na(wine$Sulphates_IMP))] <- mean(wine$Sulphates_IMP,na.rm = TRUE)
wine$Alcohol_IMP[which(is.na(wine$Alcohol_IMP))] <- mean(wine$Alcohol_IMP,na.rm = TRUE)
wine$LabelAppeal_IMP[which(is.na(wine$LabelAppeal_IMP))] <- mean(wine$LabelAppeal_IMP,na.rm = TRUE)
wine$AcidIndex_IMP[which(is.na(wine$AcidIndex_IMP))] <- mean(wine$AcidIndex_IMP,na.rm = TRUE)
wine$STARS_IMP[which(is.na(wine$STARS_IMP))] <- mean(wine$STARS_IMP,na.rm = TRUE)

#replace NA's in each column with median since regression approach is not showing any value (wine_test)
wine_test$FixedAcidity_IMP[which(is.na(wine_test$FixedAcidity_IMP))] <- mean(wine_test$FixedAcidity_IMP,na.rm = TRUE)
wine_test$VolatileAcidity_IMP[which(is.na(wine_test$VolatileAcidity_IMP))] <- mean(wine_test$VolatileAcidity_IMP,na.rm = TRUE)
wine_test$CitricAcid_IMP[which(is.na(wine_test$CitricAcid_IMP))] <- mean(wine_test$CitricAcid_IMP,na.rm = TRUE)
wine_test$ResidualSugar_IMP[which(is.na(wine_test$ResidualSugar_IMP))] <- mean(wine_test$ResidualSugar_IMP,na.rm = TRUE)
wine_test$Chlorides_IMP[which(is.na(wine_test$Chlorides_IMP))] <- mean(wine_test$Chlorides_IMP,na.rm = TRUE)
wine_test$FreeSulfurDioxide_IMP[which(is.na(wine_test$FreeSulfurDioxide_IMP))] <- mean(wine_test$FreeSulfurDioxide_IMP,na.rm = TRUE)
wine_test$TotalSulfurDioxide_IMP[which(is.na(wine_test$TotalSulfurDioxide_IMP))] <- mean(wine_test$TotalSulfurDioxide_IMP,na.rm = TRUE)
wine_test$Density_IMP[which(is.na(wine_test$Density_IMP))] <- mean(wine_test$Density_IMP,na.rm = TRUE)
wine_test$pH_IMP[which(is.na(wine_test$pH_IMP))] <- mean(wine_test$pH_IMP,na.rm = TRUE)
wine_test$Sulphates_IMP[which(is.na(wine_test$Sulphates_IMP))] <- mean(wine_test$Sulphates_IMP,na.rm = TRUE)
wine_test$Alcohol_IMP[which(is.na(wine_test$Alcohol_IMP))] <- mean(wine_test$Alcohol_IMP,na.rm = TRUE)
wine_test$LabelAppeal_IMP[which(is.na(wine_test$LabelAppeal_IMP))] <- mean(wine_test$LabelAppeal_IMP,na.rm = TRUE)
wine_test$AcidIndex_IMP[which(is.na(wine_test$AcidIndex_IMP))] <- mean(wine_test$AcidIndex_IMP,na.rm = TRUE)
wine_test$STARS_IMP[which(is.na(wine_test$STARS_IMP))] <- mean(wine_test$STARS_IMP,na.rm = TRUE)

#flag NA values with new field...first, create new field...second, replace NA's with 1 else 0 (wine)
wine$ResidualSugar_IMP_Flag <- wine$ResidualSugar
wine$Chlorides_IMP_Flag <- wine$Chlorides
wine$FreeSulfurDioxide_IMP_Flag <- wine$FreeSulfurDioxide
wine$TotalSulfurDioxide_IMP_Flag <- wine$TotalSulfurDioxide
wine$pH_IMP_Flag <- wine$pH
wine$Sulphates_IMP_Flag <- wine$Sulphates
wine$Alcohol_IMP_Flag <- wine$Alcohol
wine$STARS_IMP_Flag <- wine$STARS


#flag NA values with new field...first, create new field...second, replace NA's with 1 else 0 (wine_test)
wine_test$ResidualSugar_IMP_Flag <- wine_test$ResidualSugar
wine_test$Chlorides_IMP_Flag <- wine_test$Chlorides
wine_test$FreeSulfurDioxide_IMP_Flag <- wine_test$FreeSulfurDioxide
wine_test$TotalSulfurDioxide_IMP_Flag <- wine_test$TotalSulfurDioxide
wine_test$pH_IMP_Flag <- wine_test$pH
wine_test$Sulphates_IMP_Flag <- wine_test$Sulphates
wine_test$Alcohol_IMP_Flag <- wine_test$Alcohol
wine_test$STARS_IMP_Flag <- wine_test$STARS


#NEW BINARY FIELDS TO SHOW na's (wine)
wine$ResidualSugar_IMP_Flag <- ifelse(is.na(wine$ResidualSugar_IMP_Flag)==TRUE, 1,0) 
wine$Chlorides_IMP_Flag <- ifelse(is.na(wine$Chlorides_IMP_Flag)==TRUE, 1,0)
wine$FreeSulfurDioxide_IMP_Flag <- ifelse(is.na(wine$FreeSulfurDioxide_IMP_Flag)==TRUE, 1,0)
wine$TotalSulfurDioxide_IMP_Flag <- ifelse(is.na(wine$TotalSulfurDioxide_IMP_Flag)==TRUE, 1,0)
wine$pH_IMP_Flag <- ifelse(is.na(wine$pH_IMP_Flag)==TRUE, 1,0)
wine$Sulphates_IMP_Flag <- ifelse(is.na(wine$Sulphates_IMP_Flag)==TRUE, 1,0)
wine$Alcohol_IMP_Flag <- ifelse(is.na(wine$Alcohol_IMP_Flag)==TRUE, 1,0)
wine$STARS_IMP_Flag <- ifelse(is.na(wine$STARS_IMP_Flag)==TRUE, 1,0) #LOOK FOR MISSING STAR OBSERVATIONS

#NEW BINARY FIELDS TO SHOW na's (wine_test)
wine_test$ResidualSugar_IMP_Flag <- ifelse(is.na(wine_test$ResidualSugar_IMP_Flag)==TRUE, 1,0) 
wine_test$Chlorides_IMP_Flag <- ifelse(is.na(wine_test$Chlorides_IMP_Flag)==TRUE, 1,0)
wine_test$FreeSulfurDioxide_IMP_Flag <- ifelse(is.na(wine_test$FreeSulfurDioxide_IMP_Flag)==TRUE, 1,0)
wine_test$TotalSulfurDioxide_IMP_Flag <- ifelse(is.na(wine_test$TotalSulfurDioxide_IMP_Flag)==TRUE, 1,0)
wine_test$pH_IMP_Flag <- ifelse(is.na(wine_test$pH_IMP_Flag)==TRUE, 1,0)
wine_test$Sulphates_IMP_Flag <- ifelse(is.na(wine_test$Sulphates_IMP_Flag)==TRUE, 1,0)
wine_test$Alcohol_IMP_Flag <- ifelse(is.na(wine_test$Alcohol_IMP_Flag)==TRUE, 1,0)
wine_test$STARS_IMP_Flag <- ifelse(is.na(wine_test$STARS_IMP_Flag)==TRUE, 1,0) #LOOK FOR MISSING STAR OBSERVATIONS


#make new indicator that indicates red vs white based on volatile acidity (wine)
wine$VolatileAcidity_IMP_REDFLAG <- ifelse(wine$VolatileAcidity_IMP > mean(wine$VolatileAcidity_IMP),1,0)
wine$ResidualSugar_IMP_REDFLAG <- ifelse(wine$ResidualSugar_IMP < mean(wine$ResidualSugar_IMP),1,0)
wine$TotalSulfurDioxide_IMP_REDFLAG <- ifelse(wine$TotalSulfurDioxide_IMP < mean(wine$TotalSulfurDioxide_IMP),1,0)
wine$Density_IMP_REDFLAG <- ifelse(wine$Density_IMP > mean(wine$Density_IMP),1,0)
wine$TallyUp <- wine$VolatileAcidity_IMP_REDFLAG + wine$ResidualSugar_IMP_REDFLAG + wine$TotalSulfurDioxide_IMP_REDFLAG + wine$Density_IMP_REDFLAG
wine$Final_REDFLAG <- ifelse(wine$TallyUp > mean(wine$TallyUp),1,0)

#make new indicator that indicates red vs white based on volatile acidity (wine_test)
wine_test$VolatileAcidity_IMP_REDFLAG <- ifelse(wine_test$VolatileAcidity_IMP > mean(wine_test$VolatileAcidity_IMP),1,0)
wine_test$ResidualSugar_IMP_REDFLAG <- ifelse(wine_test$ResidualSugar_IMP < mean(wine_test$ResidualSugar_IMP),1,0)
wine_test$TotalSulfurDioxide_IMP_REDFLAG <- ifelse(wine_test$TotalSulfurDioxide_IMP < mean(wine_test$TotalSulfurDioxide_IMP),1,0)
wine_test$Density_IMP_REDFLAG <- ifelse(wine_test$Density_IMP > mean(wine_test$Density_IMP),1,0)
wine_test$TallyUp <- wine_test$VolatileAcidity_IMP_REDFLAG + wine_test$ResidualSugar_IMP_REDFLAG + wine_test$TotalSulfurDioxide_IMP_REDFLAG + wine_test$Density_IMP_REDFLAG
wine_test$Final_REDFLAG <- ifelse(wine_test$TallyUp > mean(wine_test$TallyUp),1,0)

#Add Target Flag for 0 sale scenarios (wine)
wine$TARGET_Flag <- ifelse(wine$TARGET >0,1,0)
wine$TARGET_AMT <- wine$TARGET - 1
wine$TARGET_AMT <- ifelse(wine$TARGET_Flag == 0,NA,wine$TARGET-1)

# Again, double-checking to make sure we don't have any NA's in our Test Data Set
summary(wine_test)

##########################################################################################
##########################################################################################
## CHAMPION MODEL ... ZERO INFLATED POISSON (ZIP)############################################
##########################################################################################

AIC(lm_fit)
AIC(lm_fit_stepwise)
AIC(poisson_model)
AIC(NBR_Model)
AIC(ZIP_Model)
AIC(ZINB_Model)


wine_test$TARGET <- predict(ZIP_Model, newdata = wine_test, type = "response")

summary(wine_test)

select <- dplyr::select

# Scored Data File
scores <- wine_test[c("INDEX","TARGET")]
write.csv(scores, file = "ZeeshanLatifi_U3_Scored1.csv", row.names = FALSE)



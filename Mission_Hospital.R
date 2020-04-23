library(dplyr)
library(psych)
library(car)
library(corrplot)
library(dplyr)
library(gmodels)
library(gplots)
setwd("C:/Vignesh/Studies/Spring 19/Healthcare Analytics/Cases/Mission Hospital")
library(readxl)
mh <- read_excel("MH.xlsx")
mh2<-read_excel("MH.xlsx")
View(mh)
str(mh)
mh$`BP-LOW` <- as.numeric(mh$`BP-LOW`)
mh2$`BP-LOW` <- as.numeric(mh$`BP-LOW`)

t <-table(mh$MALE)
ptab<-prop.table(t)
t
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Bar Plot", xlab = "Gender", ylab = "Proportion", col=c("orange", "steelblue"), ylim=c(0,90))
box()

t <-table(mh$UNMARRIED)
ptab<-prop.table(t)
t
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Bar Plot", xlab = "Marital Status", ylab = "Proportion", col=c("orange", "steelblue"), ylim=c(0,90))
box()

factor_vars <- c("MALE","UNMARRIED","ACHD","CAD-DVD","CAD-SVD","CAD-TVD",
                      "CAD-VSD","OS-ASD","other- heart","other- respiratory","other-general","other-nervous",
                      "other-tertalogy","PM-VSD","RHD","Diabetes1","Diabetes2", "hypertension1", "hypertension2", 
                      "hypertension3", "other", "AMBULANCE","TRANSFERRED", "ALERT", "ELECTIVE")

mh[,factor_vars] <-  data.frame(apply(mh[factor_vars],2, as.factor))
mh2[,factor_vars] <-  data.frame(apply(mh2[factor_vars],2, as.factor))

dropped_featrues <- c("SL.","GENDER","MARITAL STATUS","KEY COMPLAINTS -CODE","PAST MEDICAL HISTORY CODE","BODY HEIGHT",
                      "MODE OF ARRIVAL","STATE AT THE TIME OF ARRIVAL","TYPE OF ADMSN","TOTAL COST TO HOSPITAL",
                      "TOTAL LENGTH OF STAY","LENGTH OF STAY - ICU","LENGTH OF STAY- WARD","IMPLANT USED (Y/N)","IMPLANT",
                      "COST OF IMPLANT","HR PULSE","UREA")
mh[,dropped_featrues] <-  NULL
View(mh)
str(mh)

t <-table(mh$ALERT)
ptab<-prop.table(t)
t
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Bar Plot", xlab = "Marital Status", ylab = "Proportion", col=c("orange", "steelblue"), ylim=c(0,100))
box()

mh$ALERT <- NULL

str(mh)

#cost ~ weight
model_weight <- lm(`Ln(Total Cost)` ~ `BODY WEIGHT`, data=mh)
model_weight
summary(model_weight)

table(is.na(mh)) 
table(is.na(mh$MALE))
table(is.na(mh$UNMARRIED))
table(is.na(mh$ACHD))
table(is.na(mh$CREATININE)) #33 NAs
table(is.na(mh$AMBULANCE)) 
table(is.na(mh$RR))
table(is.na(mh$`BODY WEIGHT`))
table(is.na(mh$`CAD-DVD`))
table(is.na(mh$`CAD-SVD`))
table(is.na(mh$`other- heart`))
table(is.na(mh$`PM-VSD`))
table(is.na(mh$UREA)) # 13 NAs
table(is.na(mh$HB)) #2 NAs
table(is.na(mh$`HR PULSE`))
table(is.na(mh$`BP -HIGH`)) #23 NAs
table(is.na(mh$`BP-LOW`)) #23 NAs

table(is.na(mh$RR))
# mh <-na.omit(mh) # to remove NAs
View(mh)
# str(mh)

mh$AGE <- ifelse(mh$AGE <= 10 & mh$AGE >= 1,0,ifelse(mh$AGE>= 11 & mh$AGE <=25,1,ifelse(mh$AGE>=26 & mh$AGE <50,2, ifelse(mh$AGE >=50,3,-1))))
str(mh$AGE)

test_BP <- mh
test_BP$`BP -HIGH` <- ifelse(test_BP$`BP -HIGH` <= 130 , 0, 1)
test_BP$`BP-LOW` <- ifelse(test_BP$`BP-LOW` >= 60 , 0, 1)
View(test_BP)
mh <- test_BP
View(mh)
#Removing Pulse, Urea, Height & Alert 


#Chisquare test
tb1 <- table(mh$Diabetes, mh$Diabetes2)
chisq.test(tb1)

tb1 <- table(mh$hypertension3, mh$hypertension2)
chisq.test(tb1)

tb1 <- table(mh$hypertension3, mh$other)
chisq.test(tb1)

tb1 <- table(test_BP$`BP -HIGH`, test_BP$`BP-LOW`)
chisq.test(tb1)

plot(mh$ELECTIVE, mh$`Ln(Total Cost)`, main="Scatterplot Example", 
     xlab="Weight ", ylab="Cost ", pch=19)


# ANOVA 
mh.aov <- aov(mh$`Ln(Total Cost)`~mh$MALE, data=mh)
mh.aov
summary(mh.aov) # Fail to reject Null.
# The probability is high enough so that we fail to reject the null that their means are same 

# We use Tukey pairwise comparisons
mh.tk<-TukeyHSD(mh.aov)
mh.tk


# Plot means
plotmeans(mh$`Ln(Total Cost)`~mh$MALE, xlab="gender", ylab="cost", lwd=3, col="red")
# We observe means of fees to be almost same for male and female and female is comparitively less than male

mh.aov <- aov(mh$`Ln(Total Cost)`~mh$UNMARRIED, data=mh)
mh.aov
summary(mh.aov) # Fail to reject Null.
# The probability is high enough so that we fail to reject the null that their means are same 

# We use Tukey pairwise comparisons
mh.tk<-TukeyHSD(mh.aov)
mh.tk


# Plot means
plotmeans(mh$`Ln(Total Cost)`~mh$UNMARRIED, xlab="gender", ylab="cost", lwd=3, col="red")
# We observe means of fees to be almost same for male and female and female is comparitively less than male



mh.aov <- aov(mh$`Ln(Total Cost)`~mh$ACHD, data=mh)
mh.aov
summary(mh.aov) # Fail to reject Null.
# The probability is high enough so that we fail to reject the null that their means are same 

# We use Tukey pairwise comparisons
mh.tk<-TukeyHSD(mh.aov)
mh.tk


# Plot means
plotmeans(mh$`Ln(Total Cost)`~mh$ACHD, xlab="gender", ylab="cost", lwd=3, col="red")
# We observe means of fees to be almost same for male and female and female is comparitively less than male

mh.aov <- aov(mh$`Ln(Total Cost)`~mh$TRANSFERRED, data=mh)
mh.aov
summary(mh.aov) # Fail to reject Null.
# The probability is high enough so that we fail to reject the null that their means are same 

# We use Tukey pairwise comparisons
mh.tk<-TukeyHSD(mh.aov)
mh.tk


# Plot means
plotmeans(mh$`Ln(Total Cost)`~mh$TRANSFERRED, xlab="gender", ylab="cost", lwd=3, col="red")
# We observe means of fees to be almost same for male and female and female is comparitively less than male

# Remove Transferred

mh.aov <- aov(mh$`Ln(Total Cost)`~mh$ELECTIVE, data=mh)
mh.aov
summary(mh.aov) # Fail to reject Null.
# The probability is high enough so that we fail to reject the null that their means are same 

# We use Tukey pairwise comparisons
mh.tk<-TukeyHSD(mh.aov)
mh.tk


# Plot means
plotmeans(mh$`Ln(Total Cost)`~mh$ELECTIVE, xlab="gender", ylab="cost", lwd=3, col="red")
# We observe means of fees to be almost same for male and female and female is comparitively less than male



mh.aov <- aov(mh$`Ln(Total Cost)`~mh$other, data=mh)
mh.aov
summary(mh.aov) # Fail to reject Null.
# The probability is high enough so that we fail to reject the null that their means are same 

# We use Tukey pairwise comparisons
mh.tk<-TukeyHSD(mh.aov)
mh.tk


# Plot means
plotmeans(mh$`Ln(Total Cost)`~mh$other, xlab="gender", ylab="cost", lwd=3, col="red")
# We observe means of fees to be almost same for male and female and female is comparitively less than male

#Remove Diabetes1, hypertension2, hypertension3

mh.aov <- aov(mh$`Ln(Total Cost)`~mh$RHD, data=mh)
mh.aov
summary(mh.aov) # Fail to reject Null.
# The probability is high enough so that we fail to reject the null that their means are same 

# We use Tukey pairwise comparisons
mh.tk<-TukeyHSD(mh.aov)
mh.tk


# Plot means
plotmeans(mh$`Ln(Total Cost)`~mh$`OS-ASD`, xlab="gender", ylab="cost", lwd=3, col="red")

#Removing CAD-SVD,CAD-VSD,OS-ASD,other- heart,other- respiratory,other-nervous,other-tertalogy,PM-VSD

str(mh)


#Removing Pulse, Urea, Height & Alarm 
# Remove Transferred
#Remove Diabetes1, hypertension2, hypertension3
#Removing CAD-SVD,CAD-VSD,OS-ASD,other- heart,other- respiratory,other-nervous,other-tertalogy,PM-VSD

drop_vars_anova <- c("TRANSFERRED","Diabetes1","hypertension2","hypertension3","CAD-SVD","CAD-VSD","OS-ASD","other-nervous"
              ,"other-tertalogy","PM-VSD")

mh[,drop_vars_anova] <-  NULL

View(mh)

model1<- lm(`Ln(Total Cost)` ~., data=mh)
model1
summary(model1)

dropped_featrues_Qn2 <- c("SL.","GENDER","MARITAL STATUS","KEY COMPLAINTS -CODE","PAST MEDICAL HISTORY CODE","BODY HEIGHT",
                          "IMPLANT USED (Y/N)","IMPLANT","MODE OF ARRIVAL","STATE AT THE TIME OF ARRIVAL","TYPE OF ADMSN",
                          "TOTAL COST TO HOSPITAL","HR PULSE","UREA")
mh2[,dropped_featrues_Qn2] <-  NULL
View(mh2)
str(mh2)
mh2$ALERT <- NULL

mh2$AGE <- ifelse(mh2$AGE <= 10 & mh2$AGE >= 1,0,ifelse(mh2$AGE>= 11 & mh2$AGE <=25,1,ifelse(mh2$AGE>=26 & mh2$AGE <50,2, ifelse(mh2$AGE >=50,3,-1))))
str(mh2$AGE)

test_BP <- mh2
test_BP$`BP -HIGH` <- ifelse(test_BP$`BP -HIGH` <= 130 , 0, 1)
test_BP$`BP-LOW` <- ifelse(test_BP$`BP-LOW` >= 60 , 0, 1)
View(test_BP)
mh2 <- test_BP
View(mh2)
mh2[,drop_vars_anova] <-  NULL
str(mh2)

plot(mh2$`Ln(Total Cost)`~mh2$`LENGTH OF STAY- WARD`, col="red", 
     main="Relationship of duration with fees", 
     xlab="duration", 
     ylab="fees", 
     pch=16)

abline(lm(mh2$`Ln(Total Cost)`~mh2$`LENGTH OF STAY- WARD`), col="darkgreen", lwd=2.5)

lines(lowess(mh2$`Ln(Total Cost)`~mh2$`LENGTH OF STAY- WARD`), col="steelblue", lwd=2.5)

# There is a positive relation between duration and fees As duration increases the fees also increases

# It seems that one of the factor levels might be performing better for higher durations

pairs(~mh2$`TOTAL LENGTH OF STAY`+mh2$`LENGTH OF STAY - ICU`+mh2$`LENGTH OF STAY- WARD`, data=mh2, main="Correlations of Numeric Variables in the telco Data") #Diong a Scatterplot
str(mh2)

mh2num <- mh2[,c(22,23,24,25,26)] # Making a numeric dataset
View(mh2num)
cormat <- cor(mh2num) # Correlation matrix
round(cormat, 2) # Rounding off to two decimal places

corrplot(cormat, method="shade", addCoef.col="black") # Making a corrplot to find various correlations
#As observed there is correaltion between duration and fees and the corr value is 0.79

mh2$`TOTAL LENGTH OF STAY`<-NULL
str(mh2)

model2<- lm(`Ln(Total Cost)` ~., data=mh2)
model2
summary(model2)

model3<- lm(`Ln(Total Cost)` ~mh2$`LENGTH OF STAY- WARD`+mh2$`LENGTH OF STAY - ICU`+mh2$`COST OF IMPLANT`, data=mh2)
model3
summary(model3)


describeBy(mh2$`COST OF IMPLANT`, mh2$ELECTIVE)
# Mean and median across factor levels are almost same so it would be hard to find any relation.

# BoxPlot
boxplot(`COST OF IMPLANT` ~ ELECTIVE, data=mh2, main="Difference in fees by gender", 
        xlab="ELECTIVE", ylab="cost",
        col=c("orange", "lightblue4"))
# Again as we can see fees does not seem to vary much across gender 


# ANOVA 
mh2.aov <- aov(`COST OF IMPLANT`~ELECTIVE, data=mh2)
mh2.aov
summary(mh2.aov) # Fail to reject Null.
# The probability is high enough so that we fail to reject the null that their means are same 

# We use Tukey pairwise comparisons
mh2.aov.tk<-TukeyHSD(mh2.aov)
mh2.aov.tk
# None of the adjacent p values are low enough to reject the nulls.
# Thus there is no apparent relation between gender and Fees


# Plot means
plotmeans(mh2$`COST OF IMPLANT`~mh2$ELECTIVE, xlab="contract", ylab="fees", lwd=3, col="red")
# We observe means of fees to be almost same for male and female and female is comparitively less than male

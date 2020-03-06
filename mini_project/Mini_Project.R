#Package Imports
library(ggplot2)
library(dplyr)
library(statsr)
library(leaps)
library(grid)
library(gridExtra)
library(GGally)

#Load Movie Data
load("movies.Rdata")

#What variables can best predict a high critics_score?


#Cleaning Data
newData <- movies[ -c(1:2, 6:12, 14:15, 17, 19:23, 25:32)]
newData <- na.omit(newData)

str(newData)

#--Histograms for numeric values--

hist(newData$critics_score)
summary(newData$critics_score)
#Rising average of critics score generously towards 100

hist(newData$runtime)
summary(newData$runtime)
#High normal centering around 100 mins

hist(newData$imdb_rating)
summary(newData$imdb_rating)
#Normal skew right

hist(newData$audience_score)
summary(newData$audience_score)
#High score favor

ggpairs(newData, columns = 4:6)
#Correlation of numeric variables

ggplot(newData, aes(x=factor(genre), y=critics_score))+
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
#Serious advantages to Documentarys and Musical/Arts


ggplot(newData, aes(x=runtime, y=critics_score)) +
  geom_point() + stat_smooth(method=lm, level=0.95)
#Large amounts of scattering / weak linear relationship - remove runtime as well

newData2 <- newData[-c(2)]
names(newData2)

#--Building A Model--#

m_full <- lm(critics_score ~ genre + imdb_rating + audience_score + top200_box +
               mpaa_rating, data=newData2)
summary(m_full)

#NC-17:              +14 critic_score
#PG-13               -10 critic_score
#R:                   -6 critic_score
#audience_score:    +0.2 critic_score
#top200_box(yes):   +6.5 critic_score
#imdb_rating:        +15 critic_score

# *Bade Adjusted R-Squared = 0.6071*


m_1_noGENRE <- lm(critics_score ~ genre + imdb_rating + audience_score + top200_box +
                 mpaa_rating, data=newData2)
summary(m_1_noAR)$adj.r.squared
#AR2: 0.6070 - Not an improvement

m_1_noIMDB <- lm(critics_score ~ genre + audience_score + top200_box + mpaa_rating, data=newData2)
summary(m_1_noIMDB)$adj.r.squared
#AR2: 0.5390 - Def not an improvement

m_1_noASCORE <- lm(critics_score ~ genre + imdb_rating + top200_box + mpaa_rating, data=newData2)
summary(m_1_noASCORE)$adj.r.squared
#AR2: 0.602 - Nor an improvement

m_1_no200 <- lm(critics_score ~ genre + imdb_rating + audience_score + mpaa_rating, data=newData2)
summary(m_1_no200)$adj.r.squared
#AR2: 0.6065 - Almost, but no

m_1_noMPAA <- lm(critics_score ~ genre + imdb_rating + audience_score + top200_box, data=newData2)
summary(m_1_noMPAA)$adj.r.squared
#AR: 0.598 - Nope!


#No improvement on m_full, therefore we will use m_full


#--Model Diagnostics (rule of 4)--#

# 1) Checking Normality
qqnorm(m_full$residuals, main="Normality Condition")
qqline(m_full$residuals, main="Normality Condition")
#No major outliers
hist(m_full$residuals, prob=TRUE, main="Normality Condition")
#Yes, very normal dist

# 2) Checking Variability of Residuals
ggplot(data=m_full, aes(x=.fitted, y=.resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

par(mfrow=c(1,2))
plot(m_full$fitted.values, m_full$residuals)
plot(m_full$fitted.values, m_full$residuals)
# A lot of random scattering

# 3) Check for Independence
plot(m_full$residuals, main="Independency Conditions")
#Condition met- very scattered

anova(m_full)
#Genre and imdb_rating significant predictors, others not as much but still worth computation

plot(m_full$residuals ~ newData2$imdb_rating, main="Linearity Condition - imdb")
plot(m_full$residuals ~ newData2$genre, main="Linearity Condition - genre")
plot(m_full$residuals ~ newData2$top200_box, main="Linearity Condition - top200")
plot(m_full$residuals ~ newData2$audience_score, main="Linearity Condition - audience score")
plot(m_full$residuals ~ newData2$mpaa_rating, main="Linearity Condition - MPAA Rating")


#--PREDICTION--#

#Let's predict the critic_score of The Hurt Locker

genre <- "Drama"
mpaa_rating <- "R"
imdb_rating <- 7.6
audience_score <- 84
top200_box <- "no"
critics_score <- 98

TestA <- data.frame(genre, mpaa_rating, imdb_rating, audience_score, top200_box, critics_score)

prediction_JBA <- predict(m_full, newdata = TestA, interval = "confidence")
prediction_JBA
#79.9 Prediction, 98 Actual










# Library Imports
library(ggplot2)
library(dplyr)
library(statsr)
library(leaps)
library(grid)
library(gridExtra)
library(GGally)

# Load Data
load("movies.Rdata")

# Omits sections in the data we don't need
# ex: 1:2 removes column 1 to 2
dfm <- movies[ -c(1:2, 5:12, 14:15, 24:32)]
dfm <- na.omit(dfm)

#Shows list of variables left over
str(dfm)



#Histogram of audience score
hist(dfm$audience_score)

#Histogram of IMDB rating
hist(dfm$imdb_rating)

#Histogram of critics score
hist(dfm$critics_score)

#statistical Summaries of different metrics
summary(dfm$audience_score)
summary(dfm$imdb_rating)
summary(dfm$critics_score)

#Pairwise correlation between columns 3-6
#Rating variables
ggpairs(dfm, columns= 3:6)

#Pairwise correlation between columns 6-11
#Award variables
ggpairs(dfm, columns= 6:11)


#Audience score vs. Gene 
#Box Plots for each Genre as well
ggplot(dfm, aes(x=factor(genre), y=audience_score)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 60, hjust = 1))


#Audience score vs. runtime
ggplot(dfm, aes(x=runtime, y=audience_score)) +
  geom_point() + stat_smooth(method=lm, level=0.95)
# This plot shows there is a weak correlation, thus we can throw
# out the variable
dfm2 <- dfm[ -c(2, 7:11)]
#New list of variables
names(dfm2)


# -----Modeling-------#

#Adjusted R-Squared approach with backwards elimination
#How does each variable affect audience_score
m_full <- lm(audience_score ~ genre + imdb_rating + critics_score
             + audience_rating, data = dfm2)
summary(m_full)

#The model predicts a 20.18153 increase in audience score when value is Upright
#critics_score: a 0.02417 increae in audience_rating per point
#imdb_rating: a 9.38645 increase in audience_rating per point
#genre: read "Estimate Std" for each genre to see increase/decrease in 
#       audience_rating per point


#Round 1 of elimination toawrds adjusted R_squared
#Removing audience_rating
m_1_noAR <- lm(audience_score ~ genre + imdb_rating + critics_score
              , data = dfm2)
summary(m_1_noAR)$adj.r.squared
#0.7645016 is not an improvement over m_full's 0.8853

#Round 2
#Removing critics_score
m_1_noCS <- lm(audience_score ~ genre + imdb_rating + audience_rating,
               data = dfm2)
summary(m_1_noCS)$adj.r.squared
#0.8850625 is close, but not an improvement

#Round 3
#Removing IMDB Rating
m_1_noIR <- lm(audience_score ~ genre + critics_score + audience_rating,
                data = dfm2)
summary(m_1_noIR)$adj.r.squared
#0.8073704 still not an improvement

#Round 4
#Removing Genre
m_1_noGE <- lm(audience_score ~ imdb_rating + critics_score + 
                 audience_rating, data = dfm2)
summary(m_1_noGE)$adj.r.squared
#0.8824128 still not an improvement :(

## ANALYSIS - No improvement on adjusted R-Squared after elimination of variables
# so we will use our full model for our prediction!

#To validate our LR model m_full, we will check 4 assumptions
# 1) the residuals of the model are nearly normal
# 2) The variablility of the residuals is nearly constant
# 3) the residuals are independent
# 4) each variable is linearly related to the outcome


# 1 - Check normal Probablility
qqnorm(m_full$residuals, main="Normality Condition")
qqline(m_full$residuals, main="Normality Condition")
#Minor Irregularities, no serious outliers
hist(m_full$residuals, prob=TRUE, main="Normality Condition")
#Right Skewness but appears to be normal

#2 Check Variability of the residuals
ggplot(data = m_full, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

par(mfrow = c(1,2))
plot(m_full$fitted.values, m_full$residuals)
plot(m_full$fitted.values, abs(m_full$residuals))

# These show that the residuals are approx. constant?

#3 Check Independency
plot(m_full$residuals, main="Independency Conditions")
# Independent (random scattering/no pattern)

anova(m_full)
#Shows all variables are good predictors based on P-Values


#4 Check Linearity: residuals vs predictor variables
plot(m_full$residuals ~ dfm2$imdb_rating, main="Linearity Condition - imdb")
plot(m_full$residuals ~ dfm2$genre, main="Linearity Condition - genre")
plot(m_full$residuals ~ dfm2$audience_rating, main="Linearity Condition - audience_rating")
plot(m_full$residuals ~ dfm2$critics_score, main="Linearity Condition - critics_score")

#These plots show random scatter or little pattern = condition met.


#--PREDICTION--#

#We will not use the model to predict the audience score for "The Jungle Book"

genre <- "Animation"
imdb_rating <- 7.5
critics_score <- 95
audience_rating <- "Upright"
audience_score <-86
TestA <- data.frame (genre, imdb_rating, critics_score, audience_rating, audience_score)

prediction_JBA <- predict(m_full, newdata = TestA, interval="confidence")
prediction_JBA

myPrediction <- round(predict(m_full, TestA), digits=0)
c(myPrediction, TestA$audience_score)

genre <- "Other"
TestO <- data.frame(genre, imdb_rating, critics_score, audience_rating, audience_score)

prediction_JBO <- predict(m_full, newdata=TestO, interval="confidence")
prediction_JBO
myPrediction <- round(predict(m_full, TestO), digits = 0)
c(myPrediction, TestO$audience_score) #compare fitted and observed values


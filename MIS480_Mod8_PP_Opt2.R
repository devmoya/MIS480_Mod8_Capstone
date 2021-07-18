library("car")
library("caret")
library("gvlma")
library("predictmeans")
library("e1071")

data <- read.csv("data.csv")
features <- data[, c( "acousticness", "danceability", "duration_ms", "energy",          
                        "instrumentalness", "key", "liveness", "loudness", 
                        "mode", "speechiness", "tempo", "valence", 'popularity')]
lmMod <- lm(popularity~., data=features)
summary(lmMod)

#engineer target feature
features2 <- features
features2$popularityABOVE.50 <- ifelse(features2$popularity > 50, 1, 0)

mylogit <- glm(popularityABOVE.50 ~ . - popularity, data=features2, family="binomial")
probabilities <- predict(mylogit, type = "response")
features2$Predicted <- ifelse(probabilities > .5, 1, 0)
features2$Predicted <- as.factor(features2$Predicted)
features2$popularityABOVE.50 <- as.factor(features2$popularityABOVE.50)
conf_mat <- caret::confusionMatrix(features2$Predicted, features2$popularityABOVE.50)
conf_mat

precision <- 10132 / (10132+6164)

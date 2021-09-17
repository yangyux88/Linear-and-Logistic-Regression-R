##Problem 1. Linear Regression
#Q 1
home_data <- read.csv('west_roxbury_Modified_A6.csv',header=T,as.is=T)
home_data$FULL.BATH <- as.factor(home_data$FULL.BATH)
home_data$REMODEL <- as.factor(home_data$REMODEL)
View(home_data)
#Q 2
home_data$AGE <- 2021-home_data$YR.BUILT
home_data
#Q 3
home_data$High.Tax <- ifelse(home_data$TAX > median(home_data$TAX),1,0)
home_data
#Q 4
train_size <- 0.6*nrow(home_data)
set.seed(100)
train_index <- sample(x = 1:nrow(home_data), size = train_size, replace = F)
train_set <- home_data[train_index, ]
valid_set <- home_data[-train_index, ]
#Q 5
tax_reg <- lm(TAX~LIVING.AREA+AGE+FULL.BATH,train_set)
#Q 6
summary(tax_reg)
###Tax = 1.85046LIVING.AREA - 0.64054AGE + 125.70571FULL.BATH2 + 599.05182FULL.BATH3+1243.80863FULL.BATH4 + 584.00421FULL.BATH5
###We can see the coefficient estimates for each predictor are listed
###LIVING.AREA increases 1 will increase 1.85046 TAX
###AGE decreases 1 will decrease 0.64054 TAX
###Different FULL.BATH increases will increase different TAX
###The FULL.BATH is significant because it influences TAX a lot.
#Q 7
install.packages("forecast")
library(forecast)
pred_value_tax <- predict(object = tax_reg, newdata = train_set)
pred_value <- predict(object = tax_reg, newdata = valid_set)
accuracy(pred_value_tax, train_set$TAX)
accuracy(pred_value, valid_set$TAX)
#Q 8
###RMSE for training data is bigger than that for validation data
###So, there is no overfitting.

##Problem 2.Logistic Regression
#Q 1
boxplot(home_data$GROSS.AREA ~ home_data$High.Tax)
##their relationship is strong so I will use it as predictor
boxplot(home_data$AGE ~ home_data$High.Tax)
##their relationship is not strong so I will not use it as predictor
boxplot(home_data$FLOORS ~ home_data$High.Tax)
##their relationship is strong so I will use it as predictor
#Q 2
table(home_data$REMODEL,home_data$High.Tax)
##I will use it as predictor because I can see they exist relationship
#Q 3
fit_reg <- glm(formula = High.Tax ~ GROSS.AREA + REMODEL + FLOORS, 
               data = train_set, family = "binomial")
#Q 4
pred_sur_prob <- predict(object = fit_reg, valid_set,
                         type = "response")
cutoff <- 0.5
pred_sur <- ifelse(pred_sur_prob>cutoff,1,0)
pred_sur
#Q 5
summary(fit_reg)
##High.Tax = 0.0028898GROSS.AREA + 0.0644666REMODELOld + 1.1155699REMODELRECENT + 2.9305263FLOORS
#Q 6
install.packages("caret")
library(caret)
confusionMatrix(as.factor(pred_sur),
                as.factor(valid_set$High.Tax))
##Thue overall accuracy of the model is 0.8193 
#Q 7
confusionMatrix(as.factor(pred_sur),
                as.factor(valid_set$High.Tax),positive='1')
##Sensitivity is 0.8051
##Specificity is 0.8332
library(readxl)
library(dplyr)
library(psych)
library(ggplot2)
library(corrplot) #plotting correlation matrices
library(GPArotation) #methods for factor rotation
library(nFactors)  #methods for determining the number of factors
library(formattable)
#Read data 

projdata=read.csv("data_clean.csv")


#Binary Logistic Regression Model
#Convert Y into Binary variable(Trial)
y <- projdata$happy
y[y=="Quite happy"] <-"Happy"
y[y=="Very happy"] <- "Happy"
y[y=="Not at all happy"] <- "UnHappy"
y[y=="Not very happy"] <- "UnHappy"
y <- as.factor(y)
levels(y)

projdata$y<- y
projdata$y

projdata$happy = NULL

colnames(projdata)


projdata$age=as.integer(projdata$age)

summary(projdata)

projdata$comp=as.factor(projdata$comp)
projdata$sex=as.factor(projdata$sex)
projdata$edu=as.factor(projdata$edu)
projdata$employ=as.factor(projdata$employ)
projdata$employ=as.factor(projdata$employ)
projdata$rel_per=as.factor(projdata$rel_per)
projdata$rel_den=as.factor(projdata$rel_den)
projdata$social_class=as.factor(projdata$social_class)
projdata$people_trust=as.factor(projdata$people_trust)
############################################################
#Load packages
library(caret)
library(pROC)
library(plotROC)
library(ROCR)
library(pscl)
library(lmtest)
library(ResourceSelection)
library(rms)
############################################################
#Creating the Logistic Regression Model

#Interaction model(not logical to make a model with all interactions)
logmodel_inter <- glm(formula = y~(comp+sex+age+edu+employ+rel_den+rel_per+social_class+people_trust)^2 ,projdata, family= binomial(link='logit'))
summary(logmodel_inter)

#Add interaction between people trust and Gender 
logmodel3 <- glm(formula = y~.+sex:people_trust,projdata, family= binomial(link='logit'))

summary(logmodel3)


## odds ratios only
exp(coef(logmodel3))

## odds ratios and 95% CI
exp(cbind(OR = coef(logmodel3), confint(logmodel3)))
########################################
#Dsuplaying odds ratio of significant variables only
# Extracting coefficients and confidence intervals
coef_CI <- exp(cbind(OR = coef(logmodel3), confint(logmodel3)))

# Extracting p-values
p_values <- summary(logmodel3)$coefficients[, "Pr(>|z|)"]

# Getting significant variables (p < 0.05)
sig_indices <- which(p_values < 0.05)

# Displaying odds ratios for significant variables only
sig_coef_CI <- coef_CI[sig_indices, ]
sig_coef_CI

########################################


#It models the probability of the binary outcome variable 
logmodel <- glm(formula = y~.,projdata, family= binomial(link='logit'))

#Summary of the model including coefficients & statistical significance
summary(logmodel)

#Odds ratios
odds_ratios <- exp(coef(logmodel))
print(odds_ratios)
t(as.matrix(odds_ratios))
df <- as.data.frame.matrix(t(as.matrix(odds_ratios)))
library(formattable)
formattable(df)

#Analysis of deviance table
anova(logmodel)
anova(logmodel, test = "Chisq")
anova(logmodel, test ="LRT")

#Deviance based LR statistic G^2   ###########DON'T INCLUDE###########
#Fit the null model wit only an intercept
null_model <- glm(y ~ 1, family = binomial(link = 'logit'), data = na.omit(projdata))
#Saturated model
saturated_model <- glm(y ~ ., family = binomial(link = 'logit'), data = na.omit(projdata))
#Perform likelihood ratio test (null model)
lr_test_null <- lrtest(logmodel, null_model)
#View the test result
print(lr_test_null)
#Perform likelihood ratio test (saturated model)
lr_test_saturated <- lrtest(logmodel, saturated_model)
#View the test result
print(lr_test_saturated) ############ Error hena


#Predicted logit values
predictedlogit <- predict(logmodel,projdata) 
summary(predictedlogit)
#Generationg the predicted probabilities
predictedprob <- predict(logmodel, projdata, type="response")
summary(predictedprob)
# ROC plot
roc_score <- roc(projdata$y, predictedprob)
roc_curve <- plot(roc_score, main="ROC curve – Binary Logistic Regression", lwd = 2,  
                  xlab = "1 - Specificity")
axis(1, at = seq(0, 1, by = 0.1), labels = seq(1, 0, by = -0.1))
auc_value <- auc(roc_curve)

#Finding the optimal cutoff based on sensitivity and specificity
optimal_cutoff <- coords(roc_curve, "best", best.method = "closest.topleft")$threshold
optimal_cutoff


#Classification table
predictclass <- ifelse(predictedprob > optimal_cutoff, 1, 0)
CM <- table(projdata$y, predictclass)
CM
Classification_table <- as.matrix(CM)
#Error metrics and confusion matrix with Caret
#Providing metrics like like accuracy, sensitivity, specificity, etc.
confusionMatrix(factor(projdata$y), factor(predictclass))

levels(as.factor(predictclass))
 levels(loan$Prediction) <- list("0" = "Happy", "1" = "UnHappy")

# McFadden's R²
mcfadden_r2 <- pscl::pR2(logmodel)["McFadden"]

mcfadden_r2 
# VIF values for each predictor variable
vif_values <- car::vif(logmodel)
vif_values 




###################################################################
#forward model
forward <- step(logmodel3 ,direction = "forward") 
summary(forward) #similar to 1st model

#Displaying odds ratio of significant variables only
# Extracting coefficients and confidence intervals
coef_CI <- exp(cbind(OR = coef(forward), confint(forward)))

# Extracting p-values
p_values <- summary(forward)$coefficients[, "Pr(>|z|)"]

# Getting significant variables (p < 0.05)
sig_indices <- which(p_values < 0.05)

# Displaying odds ratios for significant variables only
sig_coef_CI <- coef_CI[sig_indices, ]
sig_coef_CI

##################################################################
#backward model 
attach(projdata)
backward <- step(logmodel3,direction = "backward") 
logmodel_back <- glm(formula = y ~ comp + sex + age + rel_per + rel_den + social_class, family= binomial(link='logit'))
summary(logmodel_back)


#Displaying odds ratio of significant variables only
# Extracting coefficients and confidence intervals
coef_CI <- exp(cbind(OR = coef(logmodel_back), confint(logmodel_back)))

# Extracting p-values
p_values <- summary(logmodel_back)$coefficients[, "Pr(>|z|)"]

# Getting significant variables (p < 0.05)
sig_indices <- which(p_values < 0.05)

# Displaying odds ratios for significant variables only
sig_coef_CI <- coef_CI[sig_indices, ]
sig_coef_CI


### hosmer lemshow
#install.packages("performance")
library(performance)
performance_hosmer(logmodel_back, n_bins = 10)

#Odds ratios
odds_ratios_backward <- exp(coef(logmodel_back))
print(odds_ratios_backward)

#Deviance
anova(logmodel_back, test = "Chisq")
formattable(as.data.frame.matrix(anova(logmodel_back, test = "Chisq")))

#VIF
car::vif(logmodel_back)
vif <- as.data.frame.matrix(car::vif(logmodel_back))
formattable(vif, align = c("l"))

#McFadden
pscl::pR2(logmodel_back)["McFadden"]
#Predicted logits
predictedlogit_back <- predict(logmodel_back, projdata)
summary(predictedlogit_back)
head(predictedlogit_back)
head(projdata$y)

#Predicted probabilities
predicted_probabilities_back<-predict(logmodel_back,projdata, type = 'response')
summary(predicted_probabilities_back)

#ROC curve
roc_score_back <- roc(projdata$y, predicted_probabilities_back)
roc_curve_back <- plot(roc_score_back, main="ROC curve – Backward Regression",  
                       xlab = "1 - Specificity", xlim = c(1,0), legacy.axes=T)
axis(1, at = rev(seq(0, 1, by = 0.1)), labels = rev(seq(1, 0, by = -0.1)))
auc_value_back <- auc(roc_curve_back)
auc_value_back 
#Optimal cutoff based on sensitivity and specificity
optimal_cutoff_back <- coords(roc_curve_back, "best", best.method = "closest.topleft")$threshold
oc <- coords(roc_curve_back, "best")$threshold
oc


#Classification table

projdata=read.csv("data_clean.csv")


#Binary Logistic Regression Model
#Convert Y into Binary variable(Trial)
y <- projdata$happy
y[y=="Quite happy"] <-"1"
y[y=="Very happy"] <- "1"
y[y=="Not at all happy"] <- "0"
y[y=="Not very happy"] <- "0"
y <- as.factor(y)
levels(y)

projdata$y<- y
projdata$y

projdata$happy = NULL

colnames(projdata)
projdata$y
predictclass_back <- ifelse(predicted_probabilities_back > optimal_cutoff_back, 1, 0)
CM_back <- table(projdata$y, predictclass_back)


Classification_table_back <- as.matrix(CM_back)
colnames(Classification_table_back) <- c("Actual 1", "Actual 0")
rownames(Classification_table_back) <- c("Predicted 1", "Predicted 0")
cm <- as.data.frame.matrix(Classification_table_back)
formattable(cm, align = c("l"))

#Error metrics and confusion matrix with Caret
#Providing metrics like like accuracy, sensitivity, specificity, etc.
confusionMatrix(factor(projdata$y), factor(predictclass_back))
#Compare full model & backward model
lr_test <- lrtest(logmodel_back, logmodel)




















































































































































































































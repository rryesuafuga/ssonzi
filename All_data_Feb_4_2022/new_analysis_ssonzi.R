

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(dplyr)
library(readr)


# Enter Data

All_Data_new = read_excel("All_Data_more_factors.xlsx", 
                          sheet = "All_Data_more_factors")

nrow(subset(All_Data_new, subset = (All_Data_new$Location == "Bombo" &
                All_Data_new$Per_Month_Coded == "Month 1")))

nrow(subset(All_Data_new, subset = (All_Data_new$Location == "Bombo" &
                All_Data_new$Per_Month_Coded == "Month 2")))

nrow(subset(All_Data_new, subset = (All_Data_new$Location == "Bombo" &
                All_Data_new$Per_Month_Coded == "Month 3")))


### Test normality of data

hist(All_Data_new$`Mean Speed (m/min)`)

qqnorm(All_Data_new$`Mean Speed (m/min)`, main='Mean Speed (m/min)')
qqline(All_Data_new$`Mean Speed (m/min)`)



##### --- One-sample Kolmogorov-Smirnov test
ks.test(All_Data_new$`Mean Speed (m/min)`, 'pnorm')
ks.test(All_Data_new$`Pedestrian Density (Peds/m2)`, 'pnorm')
ks.test(All_Data_new$`Pedestrian Flow (peds/m/min)`, 'pnorm')


#### --- Lilliefors (Kolmogorov-Smirnov) test for normality
nortest::lillie.test(All_Data_new$`Mean Speed (m/min)`)
nortest::lillie.test(All_Data_new$`Pedestrian Density (Peds/m2)`)
nortest::lillie.test(All_Data_new$`Pedestrian Flow (peds/m/min)`)


#### --- Anderson-Darling test for normality
nortest::ad.test(All_Data_new$`Mean Speed (m/min)`)
nortest::ad.test(All_Data_new$`Pedestrian Density (Peds/m2)`)
nortest::ad.test(All_Data_new$`Pedestrian Flow (peds/m/min)`)


#### --- Cramer-von Mises test for normality
nortest::cvm.test(All_Data_new$`Mean Speed (m/min)`)
nortest::cvm.test(All_Data_new$`Pedestrian Density (Peds/m2)`)
nortest::cvm.test(All_Data_new$`Pedestrian Flow (peds/m/min)`)


#### --- Pearson chi-square test for normality
nortest::pearson.test(All_Data_new$`Mean Speed (m/min)`)
nortest::pearson.test(All_Data_new$`Pedestrian Density (Peds/m2)`)
nortest::pearson.test(All_Data_new$`Pedestrian Flow (peds/m/min)`)



# ---- Run ANOVA or non-parametric alternative

## TWO WAY ANOVA WITHOUT INTERACTION EFFECTS
summary(anova_without_interactions <- aov(`Mean Speed (m/min)` ~ Location +
                                   Time + Sex + 
                                    Per_Hour_Coded + 
                                    Per_Month_Coded, 
                                   data = All_Data_new))


hist(residuals(anova_without_interactions))
nortest::ad.test(residuals(anova_without_interactions))

##### Not normally distributed data ......



## TWO WAY ANOVA WITH INTERACTION EFFECTS

anova_with_interactions <- aov(`Mean Speed (m/min)` ~ Location + Time 
                               + Sex + Location:Time + Location:Sex +
                                 Time:Sex, 
                                  data = All_Data_new)
summary(anova_with_interactions)



## ---- Check ANOVA assumptions: test validity?

### 1. Check the homogeneity of variance assumption
plot(anova_without_interactions, 1)

### Use the Levene's test to check the homogeneity of variances.
library(car)
car::leveneTest(`Mean Speed (m/min)` ~ Location * Time * Sex, 
                data = All_Data_new)


### Tukey's HSD


### 2. Check the normality assumption

### Normality plot of the residuals Q-Q plot
plot(anova_without_interactions, 2)

### cross-check with Shapiro-Wilk test about normality
#### --- 5000 is the maximum number of observations for this test to work.


### cross-check with Anderson-Darling test test about normality
nortest::ad.test(residuals(anova_without_interactions) )





## Split data into training data set and validation data set

n=dim(All_Data_new)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
All_Data_new_train = All_Data_new[id,]
All_Data_new_validation = All_Data_new[-id,]

library(ggplot2)

ggplot(data = All_Data_new_train, 
       aes(x = `Pedestrian Density (Peds/m2)`, 
           y = `Pedestrian Flow (peds/m/min)`)) + 
  geom_point(color='red') +
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y ~ poly(x, 2, raw = T))+
  labs(title="All data: Pedestrain Flow Vs Pedestrian Density")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


### -- three dimensions

##### with intercept included and linear model
summary(All_Data_new_unknown <- 
          lm(data = All_Data_new_train, 
             formula = `Pedestrian Flow (peds/m/min)`  ~ 
               `Pedestrian Density (Peds/m2)` +
               `Mean Speed (m/min)` ))


##### with intercept removed and linear model
summary(All_Data_new_unknown <- 
          lm(data = All_Data_new_train, 
             formula = `Pedestrian Flow (peds/m/min)`  ~ 
               0 + `Pedestrian Density (Peds/m2)` +
                   `Mean Speed (m/min)` ))

coefficients(All_Data_new_unknown)



##### with intercept removed and equation of an ellipse

summary(All_Data_new_unknown <- 
          lm(data = All_Data_new_train, 
             formula = `Pedestrian Flow (peds/m/min)`  ~ 
               0 + (`Pedestrian Density (Peds/m2)`)^2 +
               (`Mean Speed (m/min)`)^2 ))

coefficients(All_Data_new_unknown)



##### with intercept removed and polynomial of degree 2

summary(All_Data_new_unknown <- 
          lm(data = All_Data_new_train, 
             formula = `Pedestrian Flow (peds/m/min)`  ~ 
               0 + poly(`Pedestrian Density (Peds/m2)`,2, raw = T) +
               poly(`Mean Speed (m/min)`,2, raw = T)))


##### with intercept removed and polynomial of degree 2 and transformation
summary(All_Data_new_unknown <- 
          lm(data = All_Data_new_train, 
          formula = `Pedestrian Flow (peds/m/min)`  ~ 
          0 + poly(`Pedestrian Density (Peds/m2)`,2, raw = T) +
          poly(1/(log10(`Mean Speed (m/min)`)),2, raw = T)))


summary(All_Data_new_unknown <- 
          lm(data = All_Data_new_train, 
          formula = `Pedestrian Flow (peds/m/min)`  ~ 
          0 + poly(`Pedestrian Density (Peds/m2)`,2, raw = T) +
          poly(1/(sqrt(`Mean Speed (m/min)`)),2, raw = T)))

summary(All_Data_new_unknown <- 
          lm(data = All_Data_new_train, 
          formula = `Pedestrian Flow (peds/m/min)`  ~ 
          0 + poly(1/(log10(`Pedestrian Density (Peds/m2)`)),2, raw = T) +
          poly(1/(log10(`Mean Speed (m/min)`)),2, raw = T)))


coefficients(All_Data_new_unknown)


options(error = rlang::entrace)
utils:::make.packages.html(temp = TRUE)



## checking contribution of factors

#### Alternatives to ANOVA


## Scheirer-Ray-Hare test
library(rcompanion)
scheirerRayHare(Walk_Speed ~ Sex + Walkway_width_category, 
                data = alldata)




## Sunday March 27th 2022 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(dplyr)
library(readr)
library(ggplot2)
library(caret)
library(repr)

### Enter data ...

Masters_Research_Final_for_Modelling = 
  read_excel("Masters Research Final for Modelling.xls", 
              sheet = "Only_Data")



### ---- Others


summary(flow_model <- 
          lm(data = Masters_Research_Final_for_Modelling, 
             formula = Flow  ~ Speed  ))


summary(flow_model <- 
          lm(data = Masters_Research_Final_for_Modelling, 
             formula = Flow  ~ log(Speed)  ))


summary(flow_model <- 
          lm(data = Masters_Research_Final_for_Modelling, 
             formula = Flow  ~ I(Speed^-1)  ))



summary(flow_model <- 
          lm(data = Masters_Research_Final_for_Modelling, 
             formula = Flow  ~ Density  ))



Masters_Research_Final_for_Modelling$log_Speed =
  log(Masters_Research_Final_for_Modelling$Speed)

Masters_Research_Final_for_Modelling$log_Density =
  log(Masters_Research_Final_for_Modelling$Density)

Masters_Research_Final_for_Modelling$inv_Speed =
  (Masters_Research_Final_for_Modelling$Speed)^(-1)

Masters_Research_Final_for_Modelling$inv_Density =
  (Masters_Research_Final_for_Modelling$Density)^(-1)

Masters_Research_Final_for_Modelling$sq_Speed =
  (Masters_Research_Final_for_Modelling$Speed)^2

Masters_Research_Final_for_Modelling$sq_Density =
  (Masters_Research_Final_for_Modelling$Density)^2


cor(Masters_Research_Final_for_Modelling$Speed,
    Masters_Research_Final_for_Modelling$Density)



dat <- Masters_Research_Final_for_Modelling
dat <- dat[ ,-which(colnames(dat)=="inv speed")]

glimpse(dat)


#### Data Partitioning

set.seed(12345) 

index = sample(1:nrow(dat), 0.7*nrow(dat)) 

train = dat[index,] # Create the training data 
test = dat[-index,] # Create the test data

dim(train)
dim(test)


cols = colnames(dat)[ which(colnames(dat)=="Speed"):length(colnames(dat)) ]

cols = cols[-which(cols=="Flow")]

cols = cols[-which(cols=="LOS")]

cols_reg = c(cols, "Flow")



#### Scaling the Numeric Features

#### --- consider jumping scaling here ...

pre_proc_val <- preProcess(train[,cols], method = c("center", "scale"))

train[,cols] = predict(pre_proc_val, train[,cols])
test[,cols] = predict(pre_proc_val, test[,cols])

summary(train)



#### Regularization

cols_reg = c(cols, "Flow")

dummies <- dummyVars(Flow ~ ., data = dat[,cols_reg])

train_dummies = predict(dummies, newdata = train[,cols_reg])

test_dummies = predict(dummies, newdata = test[,cols_reg])

print(dim(train_dummies)); print(dim(test_dummies))


#### Ridge Regression predictions

library(glmnet)

x = as.matrix(train_dummies)
y_train = train$Flow

x_test = as.matrix(test_dummies)
y_test = test$Flow

lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)

summary(ridge_reg)


set.seed(12345)
cv_ridge <- cv.glmnet(x, y_train, alpha = 0, family = 'gaussian', lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda


# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

# Prediction and evaluation on train data
predictions_train <- predict(ridge_reg, s = optimal_lambda, newx = x)
eval_results(y_train, predictions_train, train)

# Prediction and evaluation on test data
predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = x_test)
eval_results(y_test, predictions_test, test)


summary(predictions_test); summary(predictions_train)


train$New_Flow = abs(predictions_train); test$New_Flow = abs(predictions_test)

Reconstructed_data = rbind(train,test)


summary(flow_model_rg <- 
          lm(data = Reconstructed_data, 
             formula = Flow  ~ log(Speed) ))


summary(flow_model_rg <- 
          lm(data = Reconstructed_data, 
             formula = Flow  ~ log(Density) ))





## --- LASSO REGRESSION

lambdas <- 10^seq(2, -3, by = -.1)

# Setting alpha = 1 implements lasso regression
lasso_reg <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)

# Best 
lambda_best <- lasso_reg$lambda.min 
lambda_best


lasso_model <- glmnet(x, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE)

predictions_train <- predict(lasso_model, s = lambda_best, newx = x)
eval_results(y_train, predictions_train, train)

predictions_test <- predict(lasso_model, s = lambda_best, newx = x_test)
eval_results(y_test, predictions_test, test)




## -- RANDOM FOREST

### Import libraries
library(randomForest)
library(ggplot2)

set.seed(4543)
rf.fit <- randomForest(Flow ~ ., data=dat[,cols], ntree=1000,
                       keep.forest=FALSE, importance=TRUE)



### --- INVESTIGATE BEST TRANSFORMATION

summary(flow_model_0 <- 
          lm(data = Masters_Research_Final_for_Modelling[1:4500, ], 
             formula = Flow  ~ Speed + Density  ))


trafo::assumptions(flow_model_0)



ggplot2::ggplot(data = Masters_Research_Final_for_Modelling)+
  aes(x=Speed, y=Flow, stat = "identity")+geom_point()



summary(flow_model_1 <- 
          lm(data = Masters_Research_Final_for_Modelling, 
             formula = log(Flow)  ~ 
               Speed + log_Speed + sq_Speed +
               Density + log_Density + inv_Density + sq_Density ))


summary(flow_model_1 <- 
          lm(data = Masters_Research_Final_for_Modelling, 
             formula = Flow  ~ Speed + Density + log_Speed + log_Density ))


summary(flow_model_1 <- 
          lm(data = Masters_Research_Final_for_Modelling, 
             formula = Flow  ~ 
               Speed + log_Speed + sq_Speed +
               Density + log_Density + inv_Density + sq_Density ))


Masters_Research_Final_for_Modelling$New_Flow =
  predict(flow_model_1, newdata = Masters_Research_Final_for_Modelling)

summary(Masters_Research_Final_for_Modelling$New_Flow)

summary(Masters_Research_Final_for_Modelling$Flow)


summary(flow_model_2 <- 
          lm(data = Masters_Research_Final_for_Modelling, 
             formula = New_Flow  ~ Speed + Density + log_Speed + log_Density ))


summary(flow_model_3 <- 
          lm(data = Masters_Research_Final_for_Modelling, 
             formula = New_Flow  ~ Speed + Density ))


boxplot(Masters_Research_Final_for_Modelling$Flow)

boxplot(Masters_Research_Final_for_Modelling$New_Flow)


write.csv(Masters_Research_Final_for_Modelling,
          "with_new_flow.csv")




## --- DATA SEGREGATION


LOS_A = subset(Masters_Research_Final_for_Modelling,
               subset = (LOS == "A") )


LOS_B = subset(Masters_Research_Final_for_Modelling,
               subset = (LOS == "B") )


LOS_C = subset(Masters_Research_Final_for_Modelling,
               subset = (LOS == "C") )


LOS_D = subset(Masters_Research_Final_for_Modelling,
               subset = (LOS == "D") )


LOS_E = subset(Masters_Research_Final_for_Modelling,
               subset = (LOS == "E") )


LOS_F = subset(Masters_Research_Final_for_Modelling,
               subset = (LOS == "F") )




## Sunday April 24th 2022 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(dplyr)
library(readr)
library(ggplot2)
library(caret)
library(repr)

### Enter data ...

Masters_Research_Final_for_Modelling = 
  read_excel("Masters Research Final for Modelling.xls", 
             sheet = "Only_Data")


Masters_Research_Final_for_Modelling$flow_1 =
  sapply(Masters_Research_Final_for_Modelling$Speed,
         FUN = function(x){1.52*x - (0.01*(x^2)) })

summary(Masters_Research_Final_for_Modelling$flow_1)


Masters_Research_Final_for_Modelling$flow_2 =
  sapply(Masters_Research_Final_for_Modelling$Density,
         FUN = function(x){33.99*x - (5.65*(x^2)) })

summary(Masters_Research_Final_for_Modelling$flow_2)

Masters_Research_Final_for_Modelling$new_flow = 
  apply(Masters_Research_Final_for_Modelling[,c(12,13)], 1, mean)

summary(Masters_Research_Final_for_Modelling$new_flow)


write.csv(Masters_Research_Final_for_Modelling, 
          "data_method_2.csv")


summary(flow_model <- 
          lm(data = Masters_Research_Final_for_Modelling, 
             formula = flow_1  ~ Density ))


summary(flow_model <- 
          lm(data = Masters_Research_Final_for_Modelling, 
             formula = flow_1  ~ 0 + Density + I(Density^2)))



summary(flow_model <- 
          lm(data = Masters_Research_Final_for_Modelling, 
             formula = flow_1  ~ Speed + I(Speed^2) ))


summary(flow_model <- 
          lm(data = Masters_Research_Final_for_Modelling, 
             formula = flow_1  ~ 0 + Speed + I(Speed^2) ))






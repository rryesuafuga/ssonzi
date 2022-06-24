
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# ENTER DATA
alldata <-  read.csv(file="ssonzi_data.csv", header = TRUE)
View(alldata)
alldata$Sex = as.factor(alldata$Sex)
alldata$Walkway_width_category = 
  as.factor(alldata$Walkway_width_category)

# Test for normality of the samples

Bombo = subset(alldata, Walkway_width_category == "Bombo")
Hoima = subset(alldata, Walkway_width_category == "Hoima")
Entebbe = subset(alldata, Walkway_width_category == "Entebbe")
Jinja = subset(alldata, Walkway_width_category == "Jinja")


# Descriptive Statistics

summary(Jinja$Walk_Speed)

summary(Jinja$Walk_Speed[which(Jinja$Sex == "Male")])

summary(Jinja$Walk_Speed[which(Jinja$Sex == "Female")])




## bootstrap Jinja
require(boot)
data = Jinja$Walk_Speed

BootstrapFunctionMean <- function(data=data, index){
  
  model.boot <- data[index] # We will sample along vector
  
  return( model.boot )
}


bootStrappedModel <- boot(data, BootstrapFunctionMean, R = 20000)

bootStrappedModel

str(bootStrappedModel$t[,1])

unclass(bootStrappedModel$t[,1])


boot.ci(bootStrappedModel, conf = 0.95, type = "perc")

plot(bootStrappedModel)

summary(unclass(bootStrappedModel$t[,1]))




BootstrapFunctionRegression <- function(data=Jinja_train, index){
  data <- data[index,] # We will sample along rows of data frame
  model.boot <- lm(data = data, 
                   formula = `Pedestrian Flow (peds/m/min)`  ~ 
                     0 + poly(`Pedestrian Density (Peds/m2) b`,2,raw = T))
  
  return( summary(model.boot)$r.squared )
}

bootStrappedModel <- boot(Jinja_train, BootstrapFunctionRegression, 
                          R = 5000)
bootStrappedModel

boot.ci(bootStrappedModel, conf = 0.95, type = "perc", index = 1)




# Checking distribution

shapiro.test(Bombo$Walk_Speed)
hist(Bombo$Walk_Speed)

shapiro.test(Hoima$Walk_Speed)
hist(Hoima$Walk_Speed)


# Alternatives to ANOVA

## Scheirer-Ray-Hare test
library(rcompanion)
scheirerRayHare(Walk_Speed ~ Sex + Walkway_width_category, 
                data = alldata)



# Wilcoxon-Mann-Whitney test

Bombo_Male = subset(Bombo, Sex == "Male")
Bombo_Female = subset(Bombo, Sex == "Female")
wilcox.test(x=Bombo_Male$Walk_Speed, y=Bombo_Female$Walk_Speed, 
            paired = FALSE)
            
wilcox.test(x=(subset(Hoima, Sex == "Male"))$Walk_Speed, 
            y=(subset(Hoima, Sex == "Female"))$Walk_Speed, 
            paired = FALSE)

wilcox.test(x=(subset(Entebbe, Sex == "Male"))$Walk_Speed, 
            y=(subset(Entebbe, Sex == "Female"))$Walk_Speed, 
            paired = FALSE)

wilcox.test(x=(subset(Jinja, Sex == "Male"))$Walk_Speed, 
            y=(subset(Jinja, Sex == "Female"))$Walk_Speed, 
            paired = FALSE)



# ! ----------------------------------------------------------

## transformations
hist(alldata$Walk_Speed)
shapiro.test(alldata$Walk_Speed)
shapiro.test(log(alldata$Walk_Speed))
shapiro.test(log(sqrt(alldata$Walk_Speed)))
shapiro.test(sqrt(alldata$Walk_Speed))
shapiro.test((alldata$Walk_Speed)**2)
shapiro.test(1/(alldata$Walk_Speed))
shapiro.test(asin(sqrt(alldata$Walk_Speed)))

library(geoR)
fit=boxcoxfit(alldata$Walk_Speed)
plot(fit)



## TWO WAY ANOVA WITHOUT INTERACTION EFFECTS
res.aov2 <- aov(Walk_Speed ~ Sex + Walkway_width_category, 
                data = alldata)
summary(res.aov2)


## TWO WAY ANOVA WITH INTERACTION EFFECTS
res.aov3 <- aov(Walk_Speed ~ Sex * Walkway_width_category, 
                data = alldata)
res.aov3 <- aov(Walk_Speed ~ Sex + Walkway_width_category + 
                  Sex:Walkway_width_category, 
                data = alldata)
summary(res.aov3)


## Check ANOVA assumptions: test validity?

### 1. Check the homogeneity of variance assumption
plot(res.aov3, 1)

### Use the Levene's test to check the homogeneity of variances.
library(car)
leveneTest(Walk_Speed ~ Sex * Walkway_width_category, data = alldata)


### 2. Check the normality assumption

### Normality plot of the residuals.
plot(res.aov3, 2)

### cross-check with Shapiro-Wilk test about normality
shapiro.test(x = residuals(object = res.aov3) )



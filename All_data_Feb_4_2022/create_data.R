
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(dplyr)
library(readr)


# METHOD 1
# GENERATE UNIVARIATE NORMAL DATA FROM NORMAL DISTRIBUTION

### -- Assuming normal distribution

set.seed(0781490432)
Bombo_Male = rnorm(n=(224*2*3), mean=84.3, sd=10.8); Bombo_Male
Bombo_Female = rnorm(n=(152*2*3), mean=74, sd=10.7); Bombo_Female

set.seed(0781490432)
Jinja_Male = rnorm(n=(294*2*3), mean=82.7, sd=10.9); Jinja_Male
Jinja_Female = rnorm(n=(97*2*3), mean=73.2, sd=8); Jinja_Female

set.seed(0781490432)
Entebbe_Male = rnorm(n=(241*2*3), mean=82.1, sd=12.9); Entebbe_Male
Entebbe_Female = rnorm(n=(162*2*3), mean=72.4, sd=10.8); Entebbe_Female


set.seed(0781490432)
Hoima_Male = rnorm(n=(219*2*3), mean=79.8, sd=11.6); Entebbe_Male
Hoima_Female = rnorm(n=(183*2*3), mean=70.8, sd=8.2); Entebbe_Female

data_for_walk_ways = cbind.data.frame(Bombo_Male, Bombo_Female,
                           Jinja_Male, Jinja_Female,
                           Entebbe_Male, Entebbe_Female,
                           Hoima_Male, Hoima_Female)

write.csv(data_for_walk_ways, 
          file = "data_for_walk_ways.csv", row.names = FALSE)


# ----------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(dplyr)
library(readr)


# METHOD 2
# GENERATE ALL DATA FROM MULTIVARIATE NORMAL DISTRIBUTION

library(mvtnorm)
## install.packages("mvtnorm")


## Load data ...
All_Data = read_excel("new_format_original_data.xlsx", 
                              sheet = "All_Data")


### Get Bombo data

round(nrow(subset(All_Data, Walk_way_width == "Bombo" & Time == "AM" & Sex =="M"))/
        nrow(subset(All_Data, Walk_way_width == "Bombo"))*2500,0)

round(nrow(subset(All_Data, Walk_way_width == "Bombo" & Time == "AM" & Sex =="F"))/
        nrow(subset(All_Data, Walk_way_width == "Bombo"))*2500,0)

round(nrow(subset(All_Data, Walk_way_width == "Bombo" & Time == "PM" & Sex =="M"))/
        nrow(subset(All_Data, Walk_way_width == "Bombo"))*2500,0)

round(nrow(subset(All_Data, Walk_way_width == "Bombo" & Time == "PM" & Sex =="F"))/
        nrow(subset(All_Data, Walk_way_width == "Bombo"))*2500,0)


#### Part 1
Bombo_AM_M = subset(All_Data, Walk_way_width == "Bombo" &
                      Time == "AM" &
                      Sex == "M")

cov_matrix = cov(Bombo_AM_M[,5:7])

average = apply(Bombo_AM_M[,5:7],2, median, simplify = TRUE)

average = sapply(Bombo_AM_M[,5:7], median)

set.seed(0781490432)
Bombo_AM_M_created <- rmvnorm(n=718, mean=average, sigma=cov_matrix, method="chol")

Bombo_AM_M_created = cbind.data.frame(Walk_way_width = "Bombo",
                                      Time = "AM",
                                      Sex = "M",
                                      Bombo_AM_M_created)


#### Part 2
Bombo_AM_F = subset(All_Data, Walk_way_width == "Bombo" &
                      Time == "AM" &
                      Sex == "F")

cov_matrix = cov(Bombo_AM_F[,5:7])

average = apply(Bombo_AM_F[,5:7],2, median, simplify = TRUE)

average = sapply(Bombo_AM_F[,5:7], median)

set.seed(0781490432)
Bombo_AM_F_created <- rmvnorm(n=487, mean=average, sigma=cov_matrix, method="chol")

Bombo_AM_F_created = cbind.data.frame(Walk_way_width = "Bombo",
                                      Time = "AM",
                                      Sex = "F",
                                      Bombo_AM_F_created)



#### Part 3
Bombo_PM_M = subset(All_Data, Walk_way_width == "Bombo" &
                      Time == "PM" &
                      Sex == "M")

cov_matrix = cov(Bombo_PM_M[,5:7])

average = apply(Bombo_PM_M[,5:7],2, median, simplify = TRUE)

average = sapply(Bombo_PM_M[,5:7], median)

set.seed(0781490432)
Bombo_PM_M_created <- rmvnorm(n=772, mean=average, sigma=cov_matrix, method="chol")

Bombo_PM_M_created = cbind.data.frame(Walk_way_width = "Bombo",
                                      Time = "PM",
                                      Sex = "M",
                                      Bombo_PM_M_created)



#### Part 4
Bombo_PM_F = subset(All_Data, Walk_way_width == "Bombo" &
                      Time == "PM" &
                      Sex == "F")

cov_matrix = cov(Bombo_PM_F[,5:7])

average = apply(Bombo_PM_F[,5:7],2, median, simplify = TRUE)

average = sapply(Bombo_PM_F[,5:7], median)

set.seed(0781490432)
Bombo_PM_F_created <- rmvnorm(n=519, mean=average, sigma=cov_matrix, method="chol")

Bombo_PM_F_created = cbind.data.frame(Walk_way_width = "Bombo",
                                      Time = "PM",
                                      Sex = "F",
                                      Bombo_PM_F_created)



### Get Entebbe data

length(which(subset(All_Data, Walk_way_width == "Entebbe")$Sex == "M"))/
  sum(
length(which(subset(All_Data, Walk_way_width == "Entebbe")$Sex == "M")),
length(which(subset(All_Data, Walk_way_width == "Entebbe")$Sex == "F"))  )*
  2500


#### Part 1
Entebbe_AM_M = subset(All_Data, Walk_way_width == "Entebbe" &
                      Time == "AM" &
                      Sex == "M")

cov_matrix = cov(Entebbe_AM_M[,5:7])

average = apply(Entebbe_AM_M[,5:7],2, median, simplify = TRUE)

average = sapply(Entebbe_AM_M[,5:7], median)

set.seed(0781490432)
Entebbe_AM_M_created <- rmvnorm(n=1493, mean=average, sigma=cov_matrix, method="chol")

Entebbe_AM_M_created = cbind.data.frame(Walk_way_width = "Entebbe",
                                      Time = "AM",
                                      Sex = "M",
                                      Entebbe_AM_M_created)



#### Part 2
Entebbe_AM_F = subset(All_Data, Walk_way_width == "Entebbe" &
                      Time == "AM" &
                      Sex == "F")

cov_matrix = cov(Entebbe_AM_F[,5:7])

average = apply(Entebbe_AM_F[,5:7],2, median, simplify = TRUE)

average = sapply(Entebbe_AM_F[,5:7], median)

set.seed(0781490432)
Entebbe_AM_F_created <- rmvnorm(n=1007, mean=average, sigma=cov_matrix, method="chol")

Entebbe_AM_F_created = cbind.data.frame(Walk_way_width = "Entebbe",
                                      Time = "AM",
                                      Sex = "F",
                                      Entebbe_AM_F_created)



### Get Hoima data

round(nrow(subset(All_Data, Walk_way_width == "Hoima" & Time == "AM" & Sex =="M"))/
  nrow(subset(All_Data, Walk_way_width == "Hoima"))*2500,0)

round(nrow(subset(All_Data, Walk_way_width == "Hoima" & Time == "AM" & Sex =="F"))/
  nrow(subset(All_Data, Walk_way_width == "Hoima"))*2500,0)

round(nrow(subset(All_Data, Walk_way_width == "Hoima" & Time == "PM" & Sex =="M"))/
  nrow(subset(All_Data, Walk_way_width == "Hoima"))*2500,0)

round(nrow(subset(All_Data, Walk_way_width == "Hoima" & Time == "PM" & Sex =="F"))/
  nrow(subset(All_Data, Walk_way_width == "Hoima"))*2500,0)


#### Part 1
Hoima_AM_M = subset(All_Data, Walk_way_width == "Hoima" &
                      Time == "AM" &
                      Sex == "M")

cov_matrix = cov(Hoima_AM_M[,5:7])

average = apply(Hoima_AM_M[,5:7],2, median, simplify = TRUE)

average = sapply(Hoima_AM_M[,5:7], median)

set.seed(0781490432)
Hoima_AM_M_created <- rmvnorm(n=748, mean=average, sigma=cov_matrix, method="chol")

Hoima_AM_M_created = cbind.data.frame(Walk_way_width = "Hoima",
                                      Time = "AM",
                                      Sex = "M",
                                      Hoima_AM_M_created)


#### Part 2
Hoima_AM_F = subset(All_Data, Walk_way_width == "Hoima" &
                      Time == "AM" &
                      Sex == "F")

cov_matrix = cov(Hoima_AM_F[,5:7])

average = apply(Hoima_AM_F[,5:7],2, median, simplify = TRUE)

average = sapply(Hoima_AM_F[,5:7], median)

set.seed(0781490432)
Hoima_AM_F_created <- rmvnorm(n=504, mean=average, sigma=cov_matrix, method="chol")

Hoima_AM_F_created = cbind.data.frame(Walk_way_width = "Hoima",
                                      Time = "AM",
                                      Sex = "F",
                                      Hoima_AM_F_created)



#### Part 3
Hoima_PM_M = subset(All_Data, Walk_way_width == "Hoima" &
                      Time == "PM" &
                      Sex == "M")

cov_matrix = cov(Hoima_PM_M[,5:7])

average = apply(Hoima_PM_M[,5:7],2, median, simplify = TRUE)

average = sapply(Hoima_PM_M[,5:7], median)

set.seed(0781490432)
Hoima_PM_M_created <- rmvnorm(n=680, mean=average, sigma=cov_matrix, method="chol")

Hoima_PM_M_created = cbind.data.frame(Walk_way_width = "Hoima",
                                      Time = "PM",
                                      Sex = "M",
                                      Hoima_PM_M_created)



#### Part 4
Hoima_PM_F = subset(All_Data, Walk_way_width == "Hoima" &
                      Time == "PM" &
                      Sex == "F")

cov_matrix = cov(Hoima_PM_F[,5:7])

average = apply(Hoima_PM_F[,5:7],2, median, simplify = TRUE)

average = sapply(Hoima_PM_F[,5:7], median)

set.seed(0781490432)
Hoima_PM_F_created <- rmvnorm(n=568, mean=average, sigma=cov_matrix, method="chol")

Hoima_PM_F_created = cbind.data.frame(Walk_way_width = "Hoima",
                                      Time = "PM",
                                      Sex = "F",
                                      Hoima_PM_F_created)




### Get Jinja data

round(nrow(subset(All_Data, Walk_way_width == "Jinja" & Time == "AM" & Sex =="M"))/
        nrow(subset(All_Data, Walk_way_width == "Jinja"))*2500,0)

round(nrow(subset(All_Data, Walk_way_width == "Jinja" & Time == "AM" & Sex =="F"))/
        nrow(subset(All_Data, Walk_way_width == "Jinja"))*2500,0)

round(nrow(subset(All_Data, Walk_way_width == "Jinja" & Time == "PM" & Sex =="M"))/
        nrow(subset(All_Data, Walk_way_width == "Jinja"))*2500,0)

round(nrow(subset(All_Data, Walk_way_width == "Jinja" & Time == "PM" & Sex =="F"))/
        nrow(subset(All_Data, Walk_way_width == "Jinja"))*2500,0)


#### Part 1
Jinja_AM_M = subset(All_Data, Walk_way_width == "Jinja" &
                      Time == "AM" &
                      Sex == "M")

cov_matrix = cov(Jinja_AM_M[,5:7])

average = apply(Jinja_AM_M[,5:7],2, median, simplify = TRUE)

average = sapply(Jinja_AM_M[,5:7], median)

set.seed(0781490432)
Jinja_AM_M_created <- rmvnorm(n=1500, mean=average, sigma=cov_matrix, method="chol")

Jinja_AM_M_created = cbind.data.frame(Walk_way_width = "Jinja",
                                      Time = "AM",
                                      Sex = "M",
                                      Jinja_AM_M_created)


#### Part 2
Jinja_AM_F = subset(All_Data, Walk_way_width == "Jinja" &
                      Time == "AM" &
                      Sex == "F")

cov_matrix = cov(Jinja_AM_F[,5:7])

average = apply(Jinja_AM_F[,5:7],2, median, simplify = TRUE)

average = sapply(Jinja_AM_F[,5:7], median)

set.seed(0781490432)
Jinja_AM_F_created <- rmvnorm(n=495, mean=average, sigma=cov_matrix, method="chol")

Jinja_AM_F_created = cbind.data.frame(Walk_way_width = "Jinja",
                                      Time = "AM",
                                      Sex = "F",
                                      Jinja_AM_F_created)



#### Part 3
Jinja_PM_M = subset(All_Data, Walk_way_width == "Jinja" &
                      Time == "PM" &
                      Sex == "M")

cov_matrix = cov(Jinja_PM_M[,5:7])

average = apply(Jinja_PM_M[,5:7],2, median, simplify = TRUE)

average = sapply(Jinja_PM_M[,5:7], median)

set.seed(0781490432)
Jinja_PM_M_created <- rmvnorm(n=378, mean=average, sigma=cov_matrix, method="chol")

Jinja_PM_M_created = cbind.data.frame(Walk_way_width = "Jinja",
                                      Time = "PM",
                                      Sex = "M",
                                      Jinja_PM_M_created)



#### Part 4
Jinja_PM_F = subset(All_Data, Walk_way_width == "Jinja" &
                      Time == "PM" &
                      Sex == "F")

cov_matrix = cov(Jinja_PM_F[,5:7])

average = apply(Jinja_PM_F[,5:7],2, median, simplify = TRUE)

average = sapply(Jinja_PM_F[,5:7], median)

set.seed(0781490432)
Jinja_PM_F_created <- rmvnorm(n=128, mean=average, sigma=cov_matrix, method="chol")

Jinja_PM_F_created = cbind.data.frame(Walk_way_width = "Jinja",
                                      Time = "PM",
                                      Sex = "F",
                                      Jinja_PM_F_created)


## gather all created data together

All_Data_created = rbind(Bombo_AM_M_created,
                         Bombo_AM_F_created,
                         Bombo_PM_M_created,
                         Bombo_PM_F_created,
                         Entebbe_AM_M_created,
                         Entebbe_AM_F_created,
                         Hoima_AM_M_created,
                         Hoima_AM_F_created,
                         Hoima_PM_M_created,
                         Hoima_PM_F_created,
                         Jinja_AM_M_created,
                         Jinja_AM_F_created,
                         Jinja_PM_M_created,
                         Jinja_PM_F_created)


write.csv(All_Data_created, 
          file = "All_Data_created.csv", row.names = FALSE)



# ----------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(dplyr)
library(readr)


# METHOD 3
# GENERATE MORE DATA USING NON-PARAMETRIC BOOSTRAP


### Example 2: multivariate statistic (variances and covariance)
# correlation matrix square root (with rho = 0.5)

rho <- 0.5
val <- c(sqrt(1 + rho), sqrt(1 - rho))
corsqrt <- matrix(c(val[1], -val[2], val), 2, 2) / sqrt(2)


# generate 100 bivariate observations (with rho = 0.5)
n <- 100
set.seed(1)
data <- cbind(rnorm(n), rnorm(n)) %*% corsqrt
# define statistic function
statfun <- function(x, data) {
  cmat <- cov(data[x,])
  ltri <- lower.tri(cmat, diag = TRUE)
  cvec <- cmat[ltri]
  names(cvec) <- c("var(x1)", "cov(x1,x2)", "var(x2)")
  cvec
}


# nonparametric bootstrap
library(nptest)

npbs <- np.boot(x = 1:n, statistic = statfun, data = data)
npbs


npbs$boot.dist



# -------------------------------------------------------------

## Checking the data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(dplyr)
library(readr)


All_Data_new = read_excel("All_Data_big_data_Patrick_Ssonzi.xlsx", 
                      sheet = "Big_Data")

Bombo_Male = subset(All_Data_new, Walk_way_width == "Bombo" & Sex =="M")
summary(Bombo_Male$`Mean Speed (m/min)`)
sd(Bombo_Male$`Mean Speed (m/min)`)

Bombo_Female = subset(All_Data_new, Walk_way_width == "Bombo" & Sex =="F")
summary(Bombo_Female$`Mean Speed (m/min)`)
sd(Bombo_Female$`Mean Speed (m/min)`)


### Test normality

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



#### -----------------------------------------------------------------------

#### creating more factors ???


AM_data = subset(All_Data_new, subset = (All_Data_new$Time == "AM"))

set.seed(0781490432)
AM_data$Per_Hour = 
  sample(c(1:3), size = nrow(AM_data), 
         replace = TRUE, prob=c(0.50, 0.30, 0.20))

AM_data$Per_Hour_Coded = ifelse(AM_data$Per_Hour == 1,"Hour 1",
                                ifelse(AM_data$Per_Hour == 2, "Hour 2", 
                                       "Hour 3"))


PM_data = subset(All_Data_new, subset = (All_Data_new$Time == "PM"))

set.seed(0781490432)
PM_data$Per_Hour = 
  sample(c(1:3), size = nrow(PM_data), 
         replace = TRUE, prob=c(0.20, 0.30, 0.50))

PM_data$Per_Hour_Coded = ifelse(PM_data$Per_Hour == 1,"Hour 1",
                                ifelse(PM_data$Per_Hour == 2, "Hour 2", 
                                       "Hour 3"))

All_Data_with_hourly_split = rbind(AM_data, PM_data)

All_Data_with_hourly_split$Per_Hour = c()



Bombo_data = subset(All_Data_with_hourly_split, 
                    subset = (All_Data_with_hourly_split$Location == "Bombo"))

set.seed(0781490432)
Bombo_data$Per_Month = 
  sample(c(1:3), size = nrow(Bombo_data), 
         replace = TRUE, prob=c(1/3, 1/3, 1/3))

Bombo_data$Per_Month_Coded = ifelse(Bombo_data$Per_Month == 1,"Month 1",
                                    ifelse(Bombo_data$Per_Month == 2, "Month 2", 
                                           "Month 3"))


Entebbe_data = subset(All_Data_with_hourly_split, 
                      subset = (All_Data_with_hourly_split$Location == "Entebbe"))

set.seed(0781490432)
Entebbe_data$Per_Month = 
  sample(c(1:3), size = nrow(Entebbe_data), 
         replace = TRUE, prob=c(1/3, 1/3, 1/3))

Entebbe_data$Per_Month_Coded = ifelse(Entebbe_data$Per_Month == 1,"Month 1",
                                      ifelse(Entebbe_data$Per_Month == 2, "Month 2", 
                                             "Month 3"))


Hoima_data = subset(All_Data_with_hourly_split, 
                    subset = (All_Data_with_hourly_split$Location == "Hoima"))

set.seed(0781490432)
Hoima_data$Per_Month = 
  sample(c(1:3), size = nrow(Hoima_data), 
         replace = TRUE, prob=c(1/3, 1/3, 1/3))

Hoima_data$Per_Month_Coded = ifelse(Hoima_data$Per_Month == 1,"Month 1",
                                    ifelse(Hoima_data$Per_Month == 2, "Month 2", 
                                           "Month 3"))


Jinja_data = subset(All_Data_with_hourly_split, 
                    subset = (All_Data_with_hourly_split$Location == "Jinja"))

set.seed(0781490432)
Jinja_data$Per_Month = 
  sample(c(1:3), size = nrow(Jinja_data), 
         replace = TRUE, prob=c(1/3, 1/3, 1/3))

Jinja_data$Per_Month_Coded = ifelse(Jinja_data$Per_Month == 1,"Month 1",
                                    ifelse(Jinja_data$Per_Month == 2, "Month 2", 
                                           "Month 3"))


All_Data_with_monthly_split = rbind(Bombo_data,
                                    Entebbe_data,
                                    Hoima_data,
                                    Jinja_data)

All_Data_with_monthly_split$Per_Month = c()



write.csv(All_Data_with_monthly_split, 
          file = "All_Data_more_factors.csv", row.names = FALSE)




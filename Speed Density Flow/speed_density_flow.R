
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# SPEED Vs DENSITY

library(ggplot2)
library(readxl)
library(kableExtra)
library(knitr)  #### looking for method kable
library(ggplotlyExtra)


## Enter Data
Bombo <- read_excel("Speed Density Flow.xlsx", 
                                 sheet = "Bombo-Clean")

Entebbe <- read_excel("Speed Density Flow.xlsx", 
                      sheet = "Entebbe-Clean")

Hoima <- read_excel("Speed Density Flow.xlsx", 
                    sheet = "Hoima-Clean")


Jinja <- read_excel("Speed Density Flow.xlsx", 
                    sheet = "Jinja-Clean")


## Split data into training data set and validation data set

n=dim(Bombo)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Bombo_train = Bombo[id,]
Bombo_validation = Bombo[-id,]


ggplot(data = Bombo_train, 
       aes(x = `Pedestrian Density (Peds/m2) b`, y = `(Mean Speed m/min)`)) + 
  geom_point(color='red') +
  geom_smooth(method = "lm", se = FALSE, color="black", formula = y~x)+
  labs(title="Bombo: Pedestrain Speed Vs Pedestrian Density")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))

# write.csv(Bombo_train, "Bombo_train.csv")
# write.csv(Bombo_validation, "Bombo_validation.csv")


summary(Bombo_lm <- lm(data = Bombo_train, formula = `(Mean Speed m/min)`~ `Pedestrian Density (Peds/m2) b`))

predict_Bombo_density <- predict(Bombo_lm, 
                         newdata = Bombo_validation)

mean((predict_Bombo_density - 
        Bombo_validation$`(Mean Speed m/min)`)**2)

sqrt(mean((predict_Bombo_density - 
             Bombo_validation$`(Mean Speed m/min)`)**2))


ggplot()+
  aes(y = predict_Bombo_density, 
      x = Bombo_validation$`(Mean Speed m/min)`) + 
  geom_point(color='red')+
  geom_smooth(method = "lm", se = TRUE, color="black", formula = y~x)+
  labs(title="Speed-Density Model Validation for Bombo road", 
       x="Actual Walking Speed m/min", y=
         "Predicted Walking Speed m/min)")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))
  


n=dim(Entebbe)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Entebbe_train = Entebbe[id,]
Entebbe_validation = Entebbe[-id,]


ggplot(data = Entebbe_train, aes(x = `Pedestrian Density (Peds/m2) b`, y = `(Mean Speed m/min)`)) + 
  geom_point(color='red')+
  geom_smooth(method = "lm", se = TRUE, color="black", formula = y~x)+
  labs(title="Entebbe: Pedestrain Speed Vs Pedestrian Density")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))

summary(Entebbe_lm <-lm(data = Entebbe_train, formula = `(Mean Speed m/min)`~ `Pedestrian Density (Peds/m2) b`))


### Entebbe error 
mean(( (Entebbe_pred <- predict(Entebbe_lm, 
              newdata = Entebbe_validation)) 
      - Entebbe_validation$`(Mean Speed m/min)`)**2)


ggplot()+
  aes(y = Entebbe_pred, 
      x = Entebbe_validation$`(Mean Speed m/min)`) + 
  geom_point(color='red')+
  geom_smooth(method = "lm", se = TRUE, color="black", formula = y~x)+
  labs(title="Speed-Density Model Validation for Entebbe road", 
       x="Actual Walking Speed m/min", 
       y="Predicted Walking Speed m/min")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))




n=dim(Hoima)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Hoima_train = Hoima[id,]
Hoima_validation = Hoima[-id,]


ggplot(data = Hoima_train, aes(x = `Pedestrian Density (Peds/m2) b`, y = `(Mean Speed m/min)`)) + 
  geom_point(color='black')+
  geom_smooth(method = "lm", se = TRUE, color="black", formula = y~x)+
  labs(title="Hoima: Pedestrain Speed Vs Pedestrian Density")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))

summary(Hoima_lm <-lm(data = Hoima_train, formula = `(Mean Speed m/min)`~ `Pedestrian Density (Peds/m2) b`))


### Hoima error 
mean(( (Hoima_pred <- predict(Hoima_lm, 
                newdata = Hoima_validation)) 
       - Hoima_validation$`(Mean Speed m/min)`)**2, na.rm = TRUE)



ggplot()+
  aes(y = Hoima_pred, 
      x = Hoima_validation$`(Mean Speed m/min)`) + 
  geom_point(color='red')+
  geom_smooth(method = "lm", se = TRUE, color="black", formula = y~x)+
  labs(title="Speed-Density Model Validation for Hoima road", 
       x="Actual Walking Speed m/min", 
       y="Predicted Walking Speed m/min")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))




n=dim(Jinja)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Jinja_train = Jinja[id,]
Jinja_validation = Jinja[-id,]


ggplot(data = Jinja_train, aes(x = `Pedestrian Density (Peds/m2) b`, y = `(Mean Speed m/min)`)) + 
  geom_point(color='black')+
  geom_smooth(method = "lm", se = TRUE, color="black", formula = y~x)+
  labs(title="Jinja: Pedestrain Speed Vs Pedestrian Density")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))

summary(Jinja_lm <-lm(data = Jinja_train, 
                      formula = `(Mean Speed m/min)`~ 
                        `Pedestrian Density (Peds/m2) b`))


### Jinja error 
mean(( (Jinja_pred <- predict(Jinja_lm, 
                newdata = Jinja_validation)) 
       - Jinja_validation$`(Mean Speed m/min)`)**2, na.rm = TRUE)


ggplot()+
  aes(y = Jinja_pred, 
      x = Jinja_validation$`(Mean Speed m/min)`) + 
  geom_point(color='red')+
  geom_smooth(method = "lm", se = TRUE, color="black", formula = y~x)+
  labs(title="Speed-Density Model Validation for Jinja road", 
       x="Actual Walking Speed m/min", 
       y="Predicted Walking Speed m/min")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))




### Errors
mean(( (predict(Bombo_lm, 
                newdata = Bombo_validation)) 
       - Bombo_validation$`(Mean Speed m/min)`)**2)

mean(( (predict(Entebbe_lm, 
                     newdata = Entebbe_validation)) 
            - Entebbe_validation$`(Mean Speed m/min)`)**2)

mean(( (predict(Hoima_lm, 
                newdata = Hoima_validation)) 
       - Hoima_validation$`(Mean Speed m/min)`)**2, na.rm = TRUE)

mean(( (predict(Jinja_lm, 
                newdata = Jinja_validation)) 
       - Jinja_validation$`(Mean Speed m/min)`)**2, na.rm = TRUE)


## Combined
Combined = rbind(Bombo_train, 
                 Entebbe_train, 
                 Hoima_train, 
                 Jinja_train)

View(Combined)

ggplot(data = Combined, aes(x = `Pedestrian Density (Peds/m2) b`, y = `(Mean Speed m/min)`)) + 
  geom_point(color='black')+
  geom_smooth(method = "lm", se = TRUE, color="black", formula = y~x)+
  labs(title="Combined")+
  theme(plot.title = element_text(hjust = 0.5))

summary(Combined_lm <-lm(data = Combined, formula = `(Mean Speed m/min)`~ `Pedestrian Density (Peds/m2) b`))


## Confidence Intervals

confint(lm(data = Bombo_train, formula = `(Mean Speed m/min)`~ `Pedestrian Density (Peds/m2) b`), level = .95)
confint(lm(data = Entebbe_train, formula = `(Mean Speed m/min)`~ `Pedestrian Density (Peds/m2) b`))
confint(lm(data = Hoima_train, formula = `(Mean Speed m/min)`~ `Pedestrian Density (Peds/m2) b`))
confint(lm(data = Jinja_train, formula = `(Mean Speed m/min)`~ `Pedestrian Density (Peds/m2) b`))
confint(lm(data = Combined, formula = `(Mean Speed m/min)`~ `Pedestrian Density (Peds/m2) b`), level = .95)




library(ggfortify)
autoplot(lm(data = Bombo_train, formula = 
        `(Mean Speed m/min)`~ `Pedestrian Density (Peds/m2) b`))+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))


plot(Bombo_lm, which = 1)

ggplot(Bombo_lm, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  labs(title = "Residual Vs Fitted Values Bombo") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(Entebbe_lm, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  labs(title = "Residual Vs Fitted Values Entebbe") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(Jinja_lm, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  labs(title = "Residual Vs Fitted Values Jinja") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(Hoima_lm, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  labs(title = "Residual Vs Fitted Values Hoima") +
  theme(plot.title = element_text(hjust = 0.5))



## Test for homoscedasticity
library(lmtest)
bptest(Entebbe_lm)



Entebbe_Rechecked <- read_excel("Speed Density Flow.xlsx", 
                      sheet = "Entebbe-Rechecked")
View(Entebbe_Rechecked)

ggplot(data = Entebbe_Rechecked, aes(x = `Pedestrian Density (Peds/m2) b`, y = `(Mean Speed m/min)`)) + 
  geom_point(color='black')+
  geom_smooth(method = "lm", se = TRUE, color="black", formula = y~x)+
  labs(title="Entebbe_Rechecked")+
  theme(plot.title = element_text(hjust = 0.5))

summary(Entebbe_reduced_lm <-lm(data = Entebbe_Rechecked, 
                        formula = `(Mean Speed m/min)`~ `Pedestrian Density (Peds/m2) b`))


ggplot(data = Entebbe, aes(x = `Pedestrian Density (Peds/m2) b`, y = `(Mean Speed m/min)`)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=FALSE)+
  labs(title="Entebbe")+
  theme(plot.title = element_text(hjust = 0.5))


library(ggplotlyExtra)
ggplotly(ggplot(data = Entebbe, aes(x = `Pedestrian Density (Peds/m2) b`, y = `(Mean Speed m/min)`)) + 
           geom_point(color='black')+
           geom_smooth(method = "lm", se = TRUE, color="black", formula = y~x)+
           labs(title="Entebbe")+
           theme(plot.title = element_text(hjust = 0.5)))





# ------------------------------------------------------------

# FLOW Vs DENSITY   orthogonal polynomial regression

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(readxl)
library(kableExtra)
library(ggplotlyExtra)


## Enter Data
Bombo <- read_excel("Speed Density Flow.xlsx", 
                    sheet = "Bombo-Reduced")

Entebbe <- read_excel("Speed Density Flow.xlsx", 
                      sheet = "Entebbe-Reduced")

Hoima <- read_excel("Speed Density Flow.xlsx", 
                    sheet = "Hoima-Reduced")


Jinja <- read_excel("Speed Density Flow.xlsx", 
                    sheet = "Jinja-Reduced")


colnames(Bombo)
colnames(Entebbe)
colnames(Hoima)
colnames(Jinja)

Combined_2 = rbind(Bombo,Entebbe,Hoima,Jinja)
Combined_2[,12] <- Combined_2[,12]*10


ggplot(data = Combined_2,
       aes(x = `Pedestrian Density (Peds/m2) b`^-1, 
           y = `Pedestrian Flow (peds/m/min)`))+ 
  geom_point(color='red')+
  labs(title="Combined: Pedestrain Flow Vs inverse Pedestrian Density",
       x = "Space (m2/ped)")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  scale_x_continuous(expand = c(0,0), limits = c(0,6), 
                     breaks = seq(0, 6, by = 0.4)) +
  scale_y_continuous(limits = c(15, 75), breaks = seq(15, 75, by = 5))+
  geom_vline(xintercept = c(0.75), linetype="dashed")+
  geom_vline(xintercept = c(1.4), linetype="dashed")+
  geom_vline(xintercept = c(2.2), linetype="dashed")+
  geom_vline(xintercept = c(3.7), linetype="dashed")+
  geom_vline(xintercept = c(5.6), linetype="dashed")+
  stat_smooth(method = "nls", data = Combined_2, 
              aes(y = Combined_2$`Pedestrian Flow (peds/m/min)`,
              x = 1/(Combined_2$`Pedestrian Density (Peds/m2) b`)),
              formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)


dt = cbind.data.frame(y = 1/(Combined_2$`Pedestrian Flow (peds/m/min)`), 
                      x = (Combined_2$`Pedestrian Density (Peds/m2) b`))

fit <- nls(y ~ SSasymp(x, Asym, R0, lrc), data = dt)
summary(fit)


exp(coef(fit)[["lrc"]]) #lambda



ggplot(dt, aes(x = x, y = y)) + 
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ a * exp(-S * x), 
              
              method.args = list(start = list(a = 0.1545205, S = 1.73227)), 
              se = FALSE, 
              # starting values obtained from fit above
              color = "dark red")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))




## --- Below based on Clean data

## Enter Data
Bombo_Clean = read_excel("Speed Density Flow.xlsx", 
                    sheet = "Bombo-Clean")

Entebbe_Clean = read_excel("Speed Density Flow.xlsx", 
                      sheet = "Entebbe-Clean")

Hoima_Clean = read_excel("Speed Density Flow.xlsx", 
                    sheet = "Hoima-Clean")


Jinja_Clean = read_excel("Speed Density Flow.xlsx", 
                    sheet = "Jinja-Clean")


### Combined 

Combined_3 = rbind(Bombo_Clean,Entebbe_Clean,Hoima_Clean,Jinja_Clean)
Combined_3[,12] <- Combined_3[,12]*10


ggplot(data = Combined_3,
       aes(x = `Pedestrian Density (Peds/m2) b`^-1, 
           y = `Pedestrian Flow (peds/m/min)`))+ 
  geom_point(color='red') +
  labs(title="Combined: Pedestrain Flow Vs inverse Pedestrian Density",
       x = "Space (m2/ped)")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  scale_x_continuous(expand = c(0,0), limits = c(0,6), 
                     breaks = seq(0, 6, by = 0.4)) +
  scale_y_continuous(limits = c(15, 75), breaks = seq(15, 75, by = 5))+
  geom_vline(xintercept = c(0.75), linetype="dashed")+
  geom_vline(xintercept = c(1.4), linetype="dashed")+
  geom_vline(xintercept = c(2.2), linetype="dashed")+
  geom_vline(xintercept = c(3.7), linetype="dashed")+
  geom_vline(xintercept = c(5.6), linetype="dashed")



## Split data into training data set and validation data set

n=dim(Combined_3)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Combined_3_train = Combined_3[id,]
Combined_3_validation = Combined_3[-id,]


ggplot(data = Combined_3_train, 
       aes(x = `Pedestrian Density (Peds/m2) b`, 
           y = `Pedestrian Flow (peds/m/min)`)) + 
  geom_point(color='red') +
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y ~ poly(x, 2, raw = T))+
  labs(title="All data: Pedestrain Flow Vs Pedestrian Density")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))



summary(Combined_3_poly <- lm(data = Combined_3_train, 
                              formula = `Pedestrian Flow (peds/m/min)`  ~ 
                                0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T)))

coefficients(Combined_3_poly)

confint(Combined_3_poly, level = 0.95)


predict_Combined_3_poly <- predict(Combined_3_poly, 
                                   newdata = Combined_3_validation)

### Model Validation Combined

ggplot()+
  aes(x = Combined_3_validation$`Pedestrian Flow (peds/m/min)`, 
      y = as.vector(predict_Combined_3_poly)) + 
  geom_point(color="red", position = "jitter")+
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y~x, size=0.5)+
  labs(title="Flow-Density Model Validation for All data", 
       x="Actual Pedestrian Flow  peds/m/min", 
       y="Predicted Pedestrian Flow peds/m/min")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


### -- three dimensions


summary(Combined_3_unknown <- lm(data = Combined_3_train, 
                                 formula = `Pedestrian Flow (peds/m/min)`  ~ 
                                   0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T) +
                                   poly(1/(log10(`(Mean Speed m/min)`)),2, raw = T)))

summary(Combined_3_unknown <- lm(data = Combined_3_train, 
                                 formula = `Pedestrian Flow (peds/m/min)`  ~ 
                                   0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T) +
                                   poly(1/(sqrt(`(Mean Speed m/min)`)),2, raw = T)))

summary(Combined_3_unknown <- lm(data = Combined_3_train, 
                          formula = `Pedestrian Flow (peds/m/min)`  ~ 
                  0 + poly(1/(log10(`Pedestrian Density (Peds/m2) b`)),2, raw = T) +
                  poly(1/(log10(`(Mean Speed m/min)`)),2, raw = T)))


coefficients(Combined_3_unknown)



### --- Below based on Reduced data

# BOMBO

## Split data into training data set and validation data set

Bombo[,12] <- Bombo[,12]*10
n=dim(Bombo)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Bombo_train = Bombo[id,]
Bombo_validation = Bombo[-id,]


ggplot(data = Bombo_train, 
       aes(x = `Pedestrian Density (Peds/m2) b`, 
           y = `Pedestrian Flow (peds/m/min)`)) + 
  geom_point(color='red') +
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y ~ poly(x, 2, raw = T))+
  labs(title="Bombo: Pedestrain Flow Vs Pedestrian Density")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


summary(Bombo_poly <- lm(data = Bombo_train, 
            formula = `Pedestrian Flow (peds/m/min)`  ~ 
            0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T)))

coefficients(Bombo_poly)
# coef(Bombo_poly)

confint(Bombo_poly, level = 0.95)


predict_Bombo_poly <- predict(Bombo_poly, 
                                 newdata = Bombo_validation)


### Model Validation Bombo

ggplot()+
  aes(x = Bombo_validation$`Pedestrian Flow (peds/m/min)`, 
      y = as.vector(predict_Bombo_poly)) + 
  geom_point(color="red", position = "jitter")+
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y~x, size=0.5)+
  labs(title="Flow-Density Model Validation for Bombo road", 
       x="Actual Pedestrian Flow  peds/m/min", 
       y="Predicted Pedestrian Flow peds/m/min")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



ggplot(data = Bombo,
       aes(x = `Pedestrian Density (Peds/m2) b`^-1, 
           y = `Pedestrian Flow (peds/m/min)`))+ 
  geom_point(color='red') +
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y ~ poly(x, 2, raw = T))+
  labs(title="Bombo: Pedestrain Flow Vs inverse Pedestrian Density")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



### -- three dimensions

ggplotly(
  plot_ly(Bombo, 
          x=~`(Mean Speed m/min)`, 
          y=~`Pedestrian Density (Peds/m2) b`, 
          z=~`Pedestrian Flow (peds/m/min)`) %>%
    add_markers(size=2, color = ~`Pedestrian Flow (peds/m/min)`) %>%
    layout(title="Pedestrain Flow Vs Pedestrian Density + Pedestrian Speed: Bombo")
)


ggplotly(
  plot_ly(Entebbe, 
          x=~`(Mean Speed m/min)`, 
          y=~`Pedestrian Density (Peds/m2) b`, 
          z=~`Pedestrian Flow (peds/m/min)`) %>%
    add_markers(size=2, color = ~`Pedestrian Flow (peds/m/min)`) %>%
    layout(title="Pedestrain Flow Vs Pedestrian Density + Pedestrian Speed: Entebbe")
)


ggplotly(
  plot_ly(Hoima, 
          x=~`(Mean Speed m/min)`, 
          y=~`Pedestrian Density (Peds/m2) b`, 
          z=~`Pedestrian Flow (peds/m/min)`) %>%
    add_markers(size=2, color = ~`Pedestrian Flow (peds/m/min)`) %>%
    layout(title="Pedestrain Flow Vs Pedestrian Density + Pedestrian Speed: Hoima")
)



ggplotly(
  plot_ly(Jinja, 
          x=~`(Mean Speed m/min)`, 
          y=~`Pedestrian Density (Peds/m2) b`, 
          z=~`Pedestrian Flow (peds/m/min)`) %>%
    add_markers(size=2, color = ~`Pedestrian Flow (peds/m/min)`) %>%
    layout(title="Pedestrain Flow Vs Pedestrian Density + Pedestrian Speed: Jinja")
)




summary(Bombo_poly <- lm(data = Bombo_train, 
                   formula = `Pedestrian Flow (peds/m/min)`  ~ 
                  0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T) +
                    poly(`(Mean Speed m/min)`,2, raw = T)))

coefficients(Bombo_poly)
#### -- some terms are not significant


summary(Bombo_unknown <- lm(data = Bombo_train, 
                         formula = `Pedestrian Flow (peds/m/min)`  ~ 
                          0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T) +
                          poly(1/(log10(`(Mean Speed m/min)`)),2, raw = T)))

coefficients(Bombo_unknown)

summary(Bombo_unknown <- lm(data = Bombo_train, 
                            formula = `Pedestrian Flow (peds/m/min)`  ~ 
                              0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T) +
                              poly(1/sqrt(`(Mean Speed m/min)`),2, raw = T)))


summary(Bombo_unknown <- lm(data = Bombo_train, 
                            formula = `Pedestrian Flow (peds/m/min)`  ~ 
                              0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T) +
                              poly(log10(`(Mean Speed m/min)`),2, raw = T)))

summary(Bombo_unknown <- lm(data = Bombo_train, 
                            formula = `Pedestrian Flow (peds/m/min)`  ~ 
                              0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T) +
                              poly(1/`(Mean Speed m/min)`,2, raw = T)))

summary(Bombo_unknown <- lm(data = Bombo_train, 
                            formula = `Pedestrian Flow (peds/m/min)`  ~ 
                              0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T) +
                              poly(sqrt(`(Mean Speed m/min)`),2, raw = T)))

coefficients(Bombo_unknown)


#### --- the one of inverse logarithm performs best



### Model Validation Bombo three dimension

predict_Bombo_poly_3 <- predict(Bombo_unknown, 
                                  newdata = Bombo_validation)

ggplot()+
  aes(x = Bombo_validation$`Pedestrian Flow (peds/m/min)`, 
      y = as.vector(predict_Bombo_poly_3)) + 
  geom_point(color="red", position = "jitter")+
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y~x, size=0.5)+
  labs(title="Flow = Density + Speed : Model Validation for Bombo road", 
       x="Actual Pedestrian Flow  peds/m/min", 
       y="Predicted Pedestrian Flow peds/m/min")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

min(Hoima_validation$`Pedestrian Flow (peds/m/min)`)
max(Hoima_validation$`Pedestrian Flow (peds/m/min)`)




# ENTEBBE

## Split data into training data set and validation data set

Entebbe[,12] <- Entebbe[,12]*10
n=dim(Entebbe)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Entebbe_train = Entebbe[id,]
Entebbe_validation = Entebbe[-id,]


ggplot(data = Entebbe_train, 
       aes(x = `Pedestrian Density (Peds/m2) b`, 
           y = `Pedestrian Flow (peds/m/min)`)) + 
  geom_point(color='red') +
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y ~ poly(x, 2, raw = T))+
  labs(title="Entebbe: Pedestrain Flow Vs Pedestrian Density")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))



summary(Entebbe_poly <- lm(data = Entebbe_train, 
                         formula = `Pedestrian Flow (peds/m/min)`  ~ 
                0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T)))

coefficients(Entebbe_poly)
# coef(Entebbe_poly)

confint(Entebbe_poly, level = 0.95)


predict_Entebbe_poly <- predict(Entebbe_poly, 
                              newdata = Entebbe_validation)

### Model Validation Entebbe

ggplot()+
  aes(x = Entebbe_validation$`Pedestrian Flow (peds/m/min)`, 
      y = as.vector(predict_Entebbe_poly)) + 
  geom_point(color="red", position = "jitter")+
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y~x, size=0.5)+
  labs(title="Flow-Density Model Validation for Entebbe road", 
       x="Actual Pedestrian Flow  peds/m/min", 
       y="Predicted Pedestrian Flow peds/m/min")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


### -- three dimensions


summary(Entebbe_unknown <- lm(data = Entebbe_train, 
                            formula = `Pedestrian Flow (peds/m/min)`  ~ 
                              0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T) +
                              poly(1/(log10(`(Mean Speed m/min)`)),2, raw = T)))

summary(Entebbe_unknown <- lm(data = Entebbe_train, 
                              formula = `Pedestrian Flow (peds/m/min)`  ~ 
                                0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T) +
                                poly(1/(sqrt(`(Mean Speed m/min)`)),2, raw = T)))

coefficients(Entebbe_unknown)



### Model Validation Entebbe three dimension

predict_Entebbe_poly_3 <- predict(Entebbe_unknown, 
                                newdata = Entebbe_validation)

ggplot()+
  aes(x = Entebbe_validation$`Pedestrian Flow (peds/m/min)`, 
      y = as.vector(predict_Entebbe_poly_3)) + 
  geom_point(color="red", position = "jitter")+
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y~x, size=0.5)+
  labs(title="Flow = Density + Speed : Model Validation for Entebbe road", 
       x="Actual Pedestrian Flow  peds/m/min", 
       y="Predicted Pedestrian Flow peds/m/min")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

min(Hoima_validation$`Pedestrian Flow (peds/m/min)`)
max(Hoima_validation$`Pedestrian Flow (peds/m/min)`)




# HOIMA

## Split data into training data set and validation data set

Hoima[,12] <- Hoima[,12]*10
n=dim(Hoima)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Hoima_train = Hoima[id,]
Hoima_validation = Hoima[-id,]


ggplot(data = Hoima_train, 
       aes(x = `Pedestrian Density (Peds/m2) b`, 
           y = `Pedestrian Flow (peds/m/min)`)) + 
  geom_point(color='red') +
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y ~ poly(x, 2, raw = T))+
  labs(title="Hoima: Pedestrain Flow Vs Pedestrian Density")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))



summary(Hoima_poly <- lm(data = Hoima_train, 
                formula = `Pedestrian Flow (peds/m/min)`  ~ 
              0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T)))

coefficients(Hoima_poly)
coef(Hoima_poly)

confint(Hoima_poly, level = 0.95)


predict_Hoima_poly <- predict(Hoima_poly, 
                              newdata = Hoima_validation)

### Model Validation Hoima

ggplot()+
  aes(x = Hoima_validation$`Pedestrian Flow (peds/m/min)`, 
      y = as.vector(predict_Hoima_poly)) + 
  geom_point(color="red", position = "jitter")+
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y~x, size=0.5)+
  labs(title="Flow-Density Model Validation for Hoima road", 
       x="Actual Pedestrian Flow  peds/m/min", 
       y="Predicted Pedestrian Flow peds/m/min")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


summary(Hoima_unknown <- lm(data = Hoima_train, 
                            formula = `Pedestrian Flow (peds/m/min)`  ~ 
                            0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T) +
                            poly(1/(log10(`(Mean Speed m/min)`)),2, raw = T)))

summary(Hoima_unknown <- lm(data = Hoima_train, 
                            formula = `Pedestrian Flow (peds/m/min)`  ~ 
                              0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T) +
                              poly(1/(sqrt(`(Mean Speed m/min)`)),2, raw = T)))

coefficients(Hoima_unknown)



### Model Validation Hoima three dimension

predict_Hoima_poly_3 <- predict(Hoima_unknown, 
                                newdata = Hoima_validation)

ggplot()+
  aes(x = Hoima_validation$`Pedestrian Flow (peds/m/min)`, 
      y = as.vector(predict_Hoima_poly_3)) + 
  geom_point(color="red", position = "jitter")+
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y~x, size=0.5)+
  labs(title="Flow = Density + Speed : Model Validation for Hoima road", 
       x="Actual Pedestrian Flow  peds/m/min", 
       y="Predicted Pedestrian Flow peds/m/min")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

min(Hoima_validation$`Pedestrian Flow (peds/m/min)`)
max(Hoima_validation$`Pedestrian Flow (peds/m/min)`)



# JINJA

Jinja[,12] <- Jinja[,12]*10
n=dim(Jinja)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Jinja_train = Jinja[id,]
Jinja_validation = Jinja[-id,]

ggplot(data = Jinja_train, 
       aes(x = `Pedestrian Density (Peds/m2) b`, 
           y = `Pedestrian Flow (peds/m/min)`)) + 
  geom_point(color='red') +
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y ~ poly(x, 2, raw = T))+
  labs(title="Jinja: Pedestrain Flow Vs Pedestrian Density")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))



summary(Jinja_poly <- lm(data = Jinja_train, 
                         formula = `Pedestrian Flow (peds/m/min)`  ~ 
                0 + poly(`Pedestrian Density (Peds/m2) b`,2,raw = T)))
coefficients(Jinja_poly)
confint(Jinja_poly, level = 0.95)


predict_Jinja_poly <- predict(Jinja_poly, 
                              newdata = Jinja_validation)

### Model Validation Jinja

ggplot()+
  aes(x = Jinja_validation$`Pedestrian Flow (peds/m/min)`, 
      y = as.vector(predict_Jinja_poly)) + 
  geom_point(color="red", position = "jitter")+
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y~x, size=0.5)+
  labs(title="Flow-Density Model Validation for Jinja road", 
       x="Actual Pedestrian Flow  peds/m/min", 
       y="Predicted Pedestrian Flow peds/m/min")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


summary(Jinja_poly <- lm(data = Jinja_train, 
                         formula = `Pedestrian Flow (peds/m/min)`  ~ 
                           0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T)))

coefficients(Jinja_poly)
coef(Jinja_poly)

confint(Jinja_poly, level = 0.95)


predict_Jinja_poly <- predict(Jinja_poly, 
                              newdata = Jinja_validation)

summary(Jinja_unknown <- lm(data = Jinja_train, 
                            formula = `Pedestrian Flow (peds/m/min)`  ~ 
                              0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T) +
                              poly(1/(log10(`(Mean Speed m/min)`)),2, raw = T)))

summary(Jinja_unknown <- lm(data = Jinja_train, 
                            formula = `Pedestrian Flow (peds/m/min)`  ~ 
                              0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T) +
                              poly(1/(sqrt(`(Mean Speed m/min)`)),2, raw = T)))

coefficients(Jinja_unknown)


### Model Validation Jinja three dimension

predict_Jinja_poly_3 <- predict(Jinja_unknown, 
                                newdata = Jinja_validation)

ggplot()+
  aes(x = Jinja_validation$`Pedestrian Flow (peds/m/min)`, 
      y = as.vector(predict_Jinja_poly_3)) + 
  geom_point(color="red", position = "jitter")+
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y~x, size=0.5)+
  labs(title="Flow - Density + Speed Model : Validation for Jinja road", 
       x="Actual Pedestrian Flow  peds/m/min", 
       y="Predicted Pedestrian Flow peds/m/min")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  scale_x_continuous(expand = c(0,0), limits = c(20,50), 
                    breaks = seq(20, 50, by = 5))+
  scale_y_continuous(expand = c(0,0), limits = c(20,50), 
                     breaks = seq(20, 50, by = 5))
  

min(Jinja_validation$`Pedestrian Flow (peds/m/min)`)
max(Jinja_validation$`Pedestrian Flow (peds/m/min)`)



#### ---- Hoima and Jinja are not significant



### --- Trying combined data

# COMBINED ...

## Enter Data
library(ggplot2)
library(readxl)

Bombo <- read_excel("Speed Density Flow.xlsx", 
                    sheet = "Bombo-Reduced")

Entebbe <- read_excel("Speed Density Flow.xlsx", 
                      sheet = "Entebbe-Reduced")

Hoima <- read_excel("Speed Density Flow.xlsx", 
                    sheet = "Hoima-Reduced")


Jinja <- read_excel("Speed Density Flow.xlsx", 
                    sheet = "Jinja-Reduced")


colnames(Bombo)
colnames(Entebbe)
colnames(Hoima)
colnames(Jinja)

Combined_2 = rbind(Bombo,Entebbe,Hoima,Jinja)
Combined_2[,12] <- Combined_2[,12]*10


## Split data into training data set and validation data set

n=dim(Combined_2)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Combined_2_train = Combined_2[id,]
Combined_2_validation = Combined_2[-id,]


ggplot(data = Combined_2_train, 
       aes(x = `Pedestrian Density (Peds/m2) b`, 
           y = `Pedestrian Flow (peds/m/min)`)) + 
  geom_point(color='red') +
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y ~ poly(x, 2, raw = T))+
  labs(title="All data: Pedestrain Flow Vs Pedestrian Density")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))



summary(Combined_2_poly <- lm(data = Combined_2_train, 
                           formula = `Pedestrian Flow (peds/m/min)`  ~ 
                             0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T)))

coefficients(Combined_2_poly)

confint(Combined_2_poly, level = 0.95)


predict_Combined_2_poly <- predict(Combined_2_poly, 
                                newdata = Combined_2_validation)

### Model Validation Combined

ggplot()+
  aes(x = Combined_2_validation$`Pedestrian Flow (peds/m/min)`, 
      y = as.vector(predict_Combined_2_poly)) + 
  geom_point(color="red", position = "jitter")+
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y~x, size=0.5)+
  labs(title="Flow-Density Model Validation for All data", 
       x="Actual Pedestrian Flow  peds/m/min", 
       y="Predicted Pedestrian Flow peds/m/min")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


### -- three dimensions


summary(Combined_2_unknown <- lm(data = Combined_2_train, 
                              formula = `Pedestrian Flow (peds/m/min)`  ~ 
                                0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T) +
                                poly(1/(log10(`(Mean Speed m/min)`)),2, raw = T)))

summary(Combined_2_unknown <- lm(data = Combined_2_train, 
                              formula = `Pedestrian Flow (peds/m/min)`  ~ 
                                0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T) +
                                poly(1/(sqrt(`(Mean Speed m/min)`)),2, raw = T)))

coefficients(Combined_2_unknown)


## --- Plot 3D data

library("scatterplot3d") # load

colnames(Combined_2_train)
Combined_2_selected = Combined_2_train[,c("(Mean Speed m/min)", 
                                       "Pedestrian Density (Peds/m2) b",
                                       "Pedestrian Flow (peds/m/min)")]

scatterplot3d(Combined_2_selected, 
              pch = 16,
              grid=TRUE, box=FALSE,
              main="Pedestrain Flow Vs   Pedestrian Density + Pedestrian Speed",
              xlab = "(Mean Speed m/min)",
              ylab = "Pedestrian Density (Peds/m2) b",
              zlab = "Pedestrian Flow (peds/m/min)")

legend("right", legend = colnames(Combined_2_selected),
       col =  c("#999999", "#E69F00", "#56B4E9"), pch = 16)



library(plotly)

ggplotly(
  plot_ly(Combined_2_selected, 
          x=~`(Mean Speed m/min)`, 
          y=~`Pedestrian Density (Peds/m2) b`, 
          z=~`Pedestrian Flow (peds/m/min)`) %>%
    add_markers(size=2, color = ~`Pedestrian Flow (peds/m/min)`) %>%
    layout(title="Pedestrain Flow Vs Pedestrian Density + Pedestrian Speed: Combined")
)


ggplotly(
  plot_ly(data = Combined_2_selected) %>%
  # the scatter plot of the data points 
  add_trace(x=~`(Mean Speed m/min)`, 
            y=~`Pedestrian Density (Peds/m2) b`, 
            z=~`Pedestrian Flow (peds/m/min)`,
            type="scatter3d", mode="markers",
            marker = list(color=~`Pedestrian Flow (peds/m/min)`, 
                          colorscale = c("#FFE1A1", "#683531"), 
                          opacity = 0.7, size=2)) )

ggplotly(
  plot_ly(data = Combined_2_selected) %>%
    # the scatter plot of the data points 
    add_trace(x=~`(Mean Speed m/min)`, 
              y=~`Pedestrian Density (Peds/m2) b`, 
              z=~`Pedestrian Flow (peds/m/min)`,
              type="scatter3d", mode="markers",
              marker = list(color=~`Pedestrian Flow (peds/m/min)`, 
                            opacity = 0.7, size=2)) )



p = plot_ly(Combined_2_selected, 
            x=~`(Mean Speed m/min)`, 
            y=~`Pedestrian Density (Peds/m2) b`, 
            z=~`Pedestrian Flow (peds/m/min)`) %>%
  add_markers(size=2, color = ~`Pedestrian Flow (peds/m/min)`) 

subplot(p)



### Model Validation Combined three dimension

predict_Combined_2_poly_3 <- predict(Combined_2_unknown, 
                                newdata = Combined_2_validation)

ggplot()+
  aes(x = Combined_2_validation$`Pedestrian Flow (peds/m/min)`, 
      y = as.vector(predict_Combined_2_poly_3)) + 
  geom_point(color="red", position = "jitter")+
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y~x, size=0.5)+
  labs(title="Flow - Density + Speed Model : Validation for Combined road", 
       x="Actual Pedestrian Flow  peds/m/min", 
       y="Predicted Pedestrian Flow peds/m/min")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))




## --- **********************************************



# BOOSTRAPPING

## bootstrap Bombo
require(boot)

BootstrapFunctionRegression <- function(data=Bombo_train, index){
  data <- data[index,] # We will sample along rows of data frame
  model.boot <- lm(data = data, 
                   formula = `Pedestrian Flow (peds/m/min)`  ~ 
                     0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T))
  
  return( coef(model.boot) )
}


bootStrappedModel <- boot(Bombo_train, BootstrapFunctionRegression, 
                          R = 5000)
bootStrappedModel

boot.ci(bootStrappedModel, conf = 0.95, type = "perc", index = 1)
boot.ci(bootStrappedModel, conf = 0.95, type = "perc", index = 2)

plot(bootStrappedModel, index = 1)
plot(bootStrappedModel, index = 2)


BootstrapFunctionRegression <- function(data=Bombo_train, index){
  data <- data[index,] # We will sample along rows of data frame
  model.boot <- lm(data = data, 
                   formula = `Pedestrian Flow (peds/m/min)`  ~ 
                     0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T))
  
  return( summary(model.boot)$r.squared )
}

bootStrappedModel <- boot(Bombo_train, BootstrapFunctionRegression, 
                          R = 5000)
bootStrappedModel

boot.ci(bootStrappedModel, conf = 0.95, type = "perc", index = 1)




## bootstrap Entebbe
require(boot)

BootstrapFunctionRegression <- function(data=Entebbe_train, index){
  data <- data[index,] # We will sample along rows of data frame
  model.boot <- lm(data = data, 
                   formula = `Pedestrian Flow (peds/m/min)`  ~ 
                     0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T))
  
  return( coef(model.boot) )
}


bootStrappedModel <- boot(Entebbe_train, BootstrapFunctionRegression, 
                          R = 5000)
bootStrappedModel

boot.ci(bootStrappedModel, conf = 0.95, type = "perc", index = 1)
boot.ci(bootStrappedModel, conf = 0.95, type = "perc", index = 2)

plot(bootStrappedModel, index = 1)
plot(bootStrappedModel, index = 2)


BootstrapFunctionRegression <- function(data=Entebbe_train, index){
  data <- data[index,] # We will sample along rows of data frame
  model.boot <- lm(data = data, 
                   formula = `Pedestrian Flow (peds/m/min)`  ~ 
                     0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T))
  
  return( summary(model.boot)$r.squared )
}

bootStrappedModel <- boot(Bombo_train, BootstrapFunctionRegression, 
                          R = 5000)
bootStrappedModel

boot.ci(bootStrappedModel, conf = 0.95, type = "perc", index = 1)




## bootstrap Hoima
require(boot)

BootstrapFunctionRegression <- function(data=Hoima_train, index){
  data <- data[index,] # We will sample along rows of data frame
  model.boot <- lm(data = data, 
                   formula = `Pedestrian Flow (peds/m/min)`  ~ 
                     0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T))
  
  return( coef(model.boot) )
}


bootStrappedModel <- boot(Hoima_train, BootstrapFunctionRegression, 
                          R = 5000)
bootStrappedModel

boot.ci(bootStrappedModel, conf = 0.95, type = "perc", index = 1)
boot.ci(bootStrappedModel, conf = 0.95, type = "perc", index = 2)

plot(bootStrappedModel, index = 1)
plot(bootStrappedModel, index = 2)


BootstrapFunctionRegression <- function(data=Hoima_train, index){
  data <- data[index,] # We will sample along rows of data frame
  model.boot <- lm(data = data, 
                   formula = `Pedestrian Flow (peds/m/min)`  ~ 
                     0 + poly(`Pedestrian Density (Peds/m2) b`,2, raw = T))
  
  return( summary(model.boot)$r.squared )
}

bootStrappedModel <- boot(Hoima_train, BootstrapFunctionRegression, 
                          R = 5000)
bootStrappedModel

boot.ci(bootStrappedModel, conf = 0.95, type = "perc", index = 1)




## bootstrap Jinja
require(boot)

BootstrapFunctionRegression <- function(data=Jinja_train, index){
  data <- data[index,] # We will sample along rows of data frame
  model.boot <- lm(data = data, 
                   formula = `Pedestrian Flow (peds/m/min)`  ~ 
                0 + poly(`Pedestrian Density (Peds/m2) b`,2,raw = T))
  
  return( coef(model.boot) )
}


bootStrappedModel <- boot(Jinja_train, BootstrapFunctionRegression, 
                          R = 5000)
bootStrappedModel

boot.ci(bootStrappedModel, conf = 0.95, type = "perc", index = 1)
boot.ci(bootStrappedModel, conf = 0.95, type = "perc", index = 2)

plot(bootStrappedModel, index = 1)
plot(bootStrappedModel, index = 2)



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




## Maximum values

#### Bombo
#### Max Flow
max(Bombo$`Pedestrian Flow (peds/m/min)`)
#### Max Density
Bombo$`Pedestrian Density (Peds/m2) b`[
  which.max(Bombo$`Pedestrian Flow (peds/m/min)`)]


#### Entebbe
#### Max Flow
max(Entebbe$`Pedestrian Flow (peds/m/min)`)
#### Max Density
Entebbe$`Pedestrian Density (Peds/m2) b`[
  which.max(Entebbe$`Pedestrian Flow (peds/m/min)`)]


#### Hoima
#### Max Flow
max(Hoima$`Pedestrian Flow (peds/m/min)`, na.rm = TRUE)
#### Max Density
Hoima$`Pedestrian Density (Peds/m2) b`[
  which.max(Hoima$`Pedestrian Flow (peds/m/min)`)]


#### Jinja
#### Max Flow
max(Jinja$`Pedestrian Flow (peds/m/min)`, na.rm = TRUE)
#### Max Density
Jinja$`Pedestrian Density (Peds/m2) b`[
  which.max(Jinja$`Pedestrian Flow (peds/m/min)`)]





# --------------------------------------------------

# FLOW Vs SPEED

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(utils::getSrcDirectory()[1])

library(ggplot2)
library(readxl)
library(kableExtra)
library(knitr)  #### looking for method kable
library(ggplotlyExtra)


## Enter Data
Bombo <- read_excel("Speed Density Flow.xlsx", 
                    sheet = "Bombo-Reduced")

Entebbe <- read_excel("Speed Density Flow.xlsx", 
                      sheet = "Entebbe-Reduced")

Hoima <- read_excel("Speed Density Flow.xlsx", 
                    sheet = "Hoima-Reduced")


Jinja <- read_excel("Speed Density Flow.xlsx", 
                    sheet = "Jinja-Reduced")


## Split data into training data set and validation data set

n=dim(Bombo)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Bombo_train = Bombo[id,]
Bombo_validation = Bombo[-id,]


n=dim(Entebbe)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Entebbe_train = Entebbe[id,]
Entebbe_validation = Entebbe[-id,]


n=dim(Hoima)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Hoima_train = Hoima[id,]
Hoima_validation = Hoima[-id,]


n=dim(Jinja)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Jinja_train = Jinja[id,]
Jinja_validation = Jinja[-id,]



## Bombo

### Graph
ggplot(data = Bombo_train, 
       aes(x = `(Mean Speed m/min)`, 
           y = `Pedestrian Flow (peds/m/min)`)) + 
  geom_point(color='red') +
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y ~ poly(x, 2, raw = T))+
  labs(title="Bombo: Pedestrain Flow Vs Pedestrian Speed")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


### Model Summary
summary(Bombo_poly <- lm(data = Bombo_train, 
      formula = `Pedestrian Flow (peds/m/min)`  ~ 
      0 + poly(`(Mean Speed m/min)`,2, raw = T)))

coefficients(Bombo_poly)
# coef(Bombo_poly)

confint(Bombo_poly, level = 0.95)

predict_Bombo_poly <- predict(Bombo_poly, 
                              newdata = Bombo_validation)


### Model Validation Bombo

ggplot()+
  aes(x = Bombo_validation$`Pedestrian Flow (peds/m/min)`, 
      y = as.vector(predict_Bombo_poly)) + 
  geom_point(color="red", position = "jitter")+
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y~x, size=0.5)+
  labs(title="Flow-Speed Model Validation for Bombo road", 
       x="Actual Pedestrian Flow  peds/m/min", 
       y="Predicted Pedestrian Flow peds/m/min")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))




## Entebbe

### Graph
ggplot(data = Entebbe_train, 
       aes(x = `(Mean Speed m/min)`, 
           y = `Pedestrian Flow (peds/m/min)`)) + 
  geom_point(color='red') +
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y ~ poly(x, 2, raw = T))+
  labs(title="Entebbe: Pedestrain Flow Vs Pedestrian Speed")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


### Model Summary
summary(Entebbe_poly <- lm(data = Entebbe_train, 
                         formula = `Pedestrian Flow (peds/m/min)`  ~ 
                           0 + poly(`(Mean Speed m/min)`,2, raw = T)))

coefficients(Entebbe_poly)
# coef(Bombo_poly)

confint(Entebbe_poly, level = 0.95)

predict_Entebbe_poly <- predict(Entebbe_poly, 
                              newdata = Entebbe_validation)


### Model Validation Entebbe

ggplot()+
  aes(x = Entebbe_validation$`Pedestrian Flow (peds/m/min)`, 
      y = as.vector(predict_Entebbe_poly)) + 
  geom_point(color="red", position = "jitter")+
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y~x, size=0.5)+
  labs(title="Flow-Speed Model Validation for Entebbe road", 
       x="Actual Pedestrian Flow  peds/m/min", 
       y="Predicted Pedestrian Flow peds/m/min")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))




## Hoima

### Graph
ggplot(data = Hoima_train, 
       aes(x = `(Mean Speed m/min)`, 
           y = `Pedestrian Flow (peds/m/min)`)) + 
  geom_point(color='red') +
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y ~ poly(x, 2, raw = T))+
  labs(title="Hoima: Pedestrain Flow Vs Pedestrian Speed")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


### Model Summary
summary(Hoima_poly <- lm(data = Hoima_train, 
                         formula = `Pedestrian Flow (peds/m/min)`  ~ 
                           0 + poly(`(Mean Speed m/min)`,2, raw = T)))

coefficients(Hoima_poly)
# coef(Bombo_poly)

confint(Hoima_poly, level = 0.95)

predict_Hoima_poly <- predict(Bombo_poly, 
                              newdata = Hoima_validation)


### Model Validation Hoima

ggplot()+
  aes(x = Hoima_validation$`Pedestrian Flow (peds/m/min)`, 
      y = as.vector(predict_Hoima_poly)) + 
  geom_point(color="red", position = "jitter")+
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y~x, size=0.5)+
  labs(title="Flow-Speed Model Validation for Hoima road", 
       x="Actual Pedestrian Flow  peds/m/min", 
       y="Predicted Pedestrian Flow peds/m/min")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))




## Jinja

### Graph
ggplot(data = Jinja_train, 
       aes(x = `(Mean Speed m/min)`, 
           y = `Pedestrian Flow (peds/m/min)`)) + 
  geom_point(color='red') +
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y ~ poly(x, 2, raw = T))+
  labs(title="Jinja: Pedestrain Flow Vs Pedestrian Speed")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


### Model Summary
summary(Jinja_poly <- lm(data = Jinja_train, 
                         formula = `Pedestrian Flow (peds/m/min)`  ~ 
                           0 + poly(`(Mean Speed m/min)`,2, raw = T)))

coefficients(Jinja_poly)
# coef(Bombo_poly)

confint(Jinja_poly, level = 0.95)

predict_Jinja_poly <- predict(Jinja_poly, 
                              newdata = Jinja_validation)


### Model Validation Jinja

ggplot()+
  aes(x = Jinja_validation$`Pedestrian Flow (peds/m/min)`, 
      y = as.vector(predict_Jinja_poly)) + 
  geom_point(color="red", position = "jitter")+
  geom_smooth(method = "lm", se = TRUE, color="black", 
              formula = y~x, size=0.5)+
  labs(title="Flow-Speed Model Validation for Jinja road", 
       x="Actual Pedestrian Flow  peds/m/min", 
       y="Predicted Pedestrian Flow peds/m/min")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



# -----------------------------------------------------------------------

# PRINCIPAL COMPONENT ANALYSIS

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(readxl)
library(kableExtra)
library(ggplotlyExtra)



## --- Below based on Clean data

## Enter Data
Bombo_Clean = read_excel("Speed Density Flow.xlsx", 
                         sheet = "Bombo-Clean")

Entebbe_Clean = read_excel("Speed Density Flow.xlsx", 
                           sheet = "Entebbe-Clean")

Hoima_Clean = read_excel("Speed Density Flow.xlsx", 
                         sheet = "Hoima-Clean")


Jinja_Clean = read_excel("Speed Density Flow.xlsx", 
                         sheet = "Jinja-Clean")


Bombo = Bombo_Clean; Entebbe = Entebbe_Clean; Hoima = Hoima_Clean;

Jinja = Jinja_Clean;

### Combined 

Combined_3 = rbind(Bombo_Clean,Entebbe_Clean,Hoima_Clean,Jinja_Clean)
Combined_3[,12] <- Combined_3[,12]*10


## Split data into training data set and validation data set

n=dim(Bombo)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Bombo_train = Bombo[id,]
Bombo_validation = Bombo[-id,]


n=dim(Entebbe)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Entebbe_train = Entebbe[id,]
Entebbe_validation = Entebbe[-id,]


n=dim(Hoima)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Hoima_train = Hoima[id,]
Hoima_validation = Hoima[-id,]


n=dim(Jinja)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Jinja_train = Jinja[id,]
Jinja_validation = Jinja[-id,]


n=dim(Combined_3)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Combined_3_train = Combined_3[id,]
Combined_3_validation = Combined_3[-id,]



### Applying principal component analysis ...

new_variables = cbind.data.frame(
          flow = Combined_3_train$`Pedestrian Flow (peds/m/min)`,
          mean_speed = Combined_3_train$`(Mean Speed m/min)`,
          mean_speed_2 = Combined_3_train$`(Mean Speed m/min)`^2,
          mean_speed_3 = Combined_3_train$`(Mean Speed m/min)`^3,
          log_speed = log(Combined_3_train$`(Mean Speed m/min)`),
          inv_speed = Combined_3_train$`(Mean Speed m/min)`^(-1),
          inv_log_speed = log(Combined_3_train$`(Mean Speed m/min)`)^(-1),
          density = Combined_3_train$`Pedestrian Density (Peds/m2) b`,
          density_2 = Combined_3_train$`Pedestrian Density (Peds/m2) b`,
          density_3 = Combined_3_train$`Pedestrian Density (Peds/m2) b`^3,
          log_density = log(Combined_3_train$`Pedestrian Density (Peds/m2) b`),
          inv_density = Combined_3_train$`Pedestrian Density (Peds/m2) b`^(-1),
          inv_log_density = log(Combined_3_train$`Pedestrian Density (Peds/m2) b`)^(-1)
          )


new_variables = new_variables[-which(is.na(new_variables$mean_speed_3)), ]

new_variables.pca <- prcomp(new_variables[,-1], center = TRUE,scale. = TRUE)

summary(new_variables.pca)

str(new_variables.pca)

# library(devtools)
# install_github("vqv/ggbiplot")

library(ggbiplot)
ggbiplot(new_variables.pca)

str(new_variables.pca$x)
new_variables.pca$x[,1:2]

str(new_variables.pca$rotation)
new_variables.pca$rotation[, 1:2]


### --- Using PCA1 and PCA2 as variables

summary(flow_pca_prediction <- 
          lm(formula = new_variables$flow  ~ 
               new_variables.pca$x[,1] + 
               new_variables.pca$x[,2]))


summary(flow_pca_prediction <- 
          lm(formula = new_variables$flow  ~ 
               0 + poly(new_variables.pca$x[,1],2, raw = T) +
               poly(new_variables.pca$x[,2],2, raw = T) ))

summary(flow_pca_prediction <- 
          lm(formula = new_variables$flow  ~ 
               poly(new_variables.pca$x[,1],2, raw = T) +
               poly(new_variables.pca$x[,2],2, raw = T) ))
























## -- Below based on Reduced Data

## Enter Data
Bombo <- read_excel("Speed Density Flow.xlsx", 
                    sheet = "Bombo-Reduced")

Entebbe <- read_excel("Speed Density Flow.xlsx", 
                      sheet = "Entebbe-Reduced")

Hoima <- read_excel("Speed Density Flow.xlsx", 
                    sheet = "Hoima-Reduced")


Jinja <- read_excel("Speed Density Flow.xlsx", 
                    sheet = "Jinja-Reduced")


colnames(Bombo)
colnames(Entebbe)
colnames(Hoima)
colnames(Jinja)


### Combined 

Combined_2 = rbind(Bombo,Entebbe,Hoima,Jinja)
Combined_2[,12] <- Combined_2[,12]*10


## Split data into training data set and validation data set


n=dim(Bombo)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Bombo_train = Bombo[id,]
Bombo_validation = Bombo[-id,]


n=dim(Entebbe)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Entebbe_train = Entebbe[id,]
Entebbe_validation = Entebbe[-id,]


n=dim(Hoima)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Hoima_train = Hoima[id,]
Hoima_validation = Hoima[-id,]


n=dim(Jinja)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Jinja_train = Jinja[id,]
Jinja_validation = Jinja[-id,]


n=dim(Combined_2)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
Combined_2_train = Combined_2[id,]
Combined_2_validation = Combined_2[-id,]


Combined_2_validation = Combined_2_validation[-105, ]



### Applying principal component analysis ...

new_variables = cbind.data.frame(
  flow = Combined_2_train$`Pedestrian Flow (peds/m/min)`,
  mean_speed = Combined_2_train$`(Mean Speed m/min)`,
  mean_speed_2 = Combined_2_train$`(Mean Speed m/min)`^2,
  mean_speed_3 = Combined_2_train$`(Mean Speed m/min)`^3,
  log_speed = log(Combined_2_train$`(Mean Speed m/min)`),
  inv_speed = Combined_2_train$`(Mean Speed m/min)`^(-1),
  inv_log_speed = log(Combined_2_train$`(Mean Speed m/min)`)^(-1),
  density = Combined_2_train$`Pedestrian Density (Peds/m2) b`,
  density_2 = Combined_2_train$`Pedestrian Density (Peds/m2) b`,
  density_3 = Combined_2_train$`Pedestrian Density (Peds/m2) b`^3,
  log_density = log(Combined_2_train$`Pedestrian Density (Peds/m2) b`),
  inv_density = Combined_2_train$`Pedestrian Density (Peds/m2) b`^(-1),
  inv_log_density = log(Combined_2_train$`Pedestrian Density (Peds/m2) b`)^(-1)
)


new_variables = new_variables[-which(is.na(new_variables$mean_speed_3)),]


new_variables.pca <- prcomp(new_variables[,-c(1)], center = TRUE,scale. = TRUE)

summary(new_variables.pca)

str(new_variables.pca)

# library(devtools)
# install_github("vqv/ggbiplot")

library(ggbiplot)
ggbiplot(new_variables.pca)

str(new_variables.pca$x)
new_variables.pca$x[,1:2]

str(new_variables.pca$rotation)
new_variables.pca$rotation[, 1:2]


### --- Using PCA1 and PCA2 as variables

summary(flow_pca_prediction <- 
          lm(formula = new_variables$flow  ~ 
               new_variables.pca$x[,1] + 
               new_variables.pca$x[,2]))


summary(flow_pca_prediction <- 
          lm(formula = new_variables$flow  ~ 
               0 + poly(new_variables.pca$x[,1],2, raw = T) +
               poly(new_variables.pca$x[,2],2, raw = T) ))

summary(flow_pca_prediction <- 
          lm(formula = new_variables$flow  ~ 
               poly(new_variables.pca$x[,1],2, raw = T) +
               poly(new_variables.pca$x[,2],2, raw = T) ))












































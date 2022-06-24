

# May 29th 2022

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(ggpubr)
library(readxl)
library(kableExtra)


## Enter Data
Masters <- 
  read_excel("Masters Research Final for Model Testing (29.05.2022) Edited.xlsx", 
                    sheet = "Validation Data")



colnames(Masters)

summary(Masters$`Space (m2/ped)`)

summary(Masters$`Space (m2/ped) based on LOS Criteria`)

summary(Masters$`Modelled Flow (ped/min/m)`)

unique(Masters$Location)



### Boxplot

ggplot(data = Masters,
       aes(x = `Space (m2/ped)`, 
           y = `Modelled Flow (ped/min/m)`))+ 
  geom_boxplot(color='red', stat = "boxplot", position = "dodge2")+
  labs(title="Boxplot Space (m2/ped)",
       x = "Space (m2/ped)")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



ggplot(data = Masters,
       aes(x = `Space (m2/ped)` ))+ 
  geom_point(y = Masters$`Modelled Flow (ped/min/m)`, colour='blue')+
  labs(title="Pedestrain Flow Vs Space (inverse Pedestrian Density)",
       x = "Space (m2/ped)",
       y = "Pedestrain Flow (peds/min/m)")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  scale_x_continuous(expand = c(0,0), limits = c(0,7), 
                     breaks = seq(0, 7, by = 0.5)) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 5))+
  geom_vline(xintercept = c(0.8), linetype="dashed")+
  geom_vline(xintercept = c(1.5), linetype="dashed")+
  geom_vline(xintercept = c(2.3), linetype="dashed")+
  geom_vline(xintercept = c(3.8), linetype="dashed")+
  geom_vline(xintercept = c(5.7), linetype="dashed")+
  geom_vline(xintercept = c(7), linetype="dashed")+
  stat_smooth(method = "nls", data = Masters, 
              aes(y = `Modelled Flow (ped/min/m)`,
                  x = `Space (m2/ped)`), colour = "green",
              formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)





### May 31st 2022


ggplot(data = Masters,
       aes(x = `Space (m2/ped)` ))+ 
  geom_point(y = Masters$`Modelled Flow (ped/min/m)`, colour='blue')+
  labs(title="Pedestrain Flow Vs Space (inverse Pedestrian Density)",
       x = "Space (m2/ped)",
       y = "Pedestrain Flow (peds/min/m)")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  scale_x_continuous(expand = c(0,0), limits = c(0,7), 
                     breaks = seq(0, 7, by = 0.5)) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 5))+
  geom_vline(xintercept = c(0.8), linetype="dashed")+
  geom_vline(xintercept = c(1.5), linetype="dashed")+
  geom_vline(xintercept = c(2.3), linetype="dashed")+
  geom_vline(xintercept = c(3.8), linetype="dashed")+
  geom_vline(xintercept = c(5.7), linetype="dashed")+
  geom_vline(xintercept = c(7), linetype="dashed")



unique(Masters$Location)

ggplot(data = Masters,
       aes(x = `Space (m2/ped)` ))+ 
  geom_point(y = Masters$`Modelled Flow (ped/min/m)`, colour='blue')+
  labs(title="Pedestrain Flow Vs Space (inverse Pedestrian Density)",
       x = "Space (m2/ped)",
       y = "Pedestrain Flow (peds/min/m)")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  scale_x_continuous(expand = c(0,0), limits = c(0,7), 
                     breaks = seq(0, 7, by = 0.5)) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 5))+
  geom_vline(xintercept = c(0.8), linetype="dashed")+
  geom_vline(xintercept = c(1.5), linetype="dashed")+
  geom_vline(xintercept = c(2.3), linetype="dashed")+
  geom_vline(xintercept = c(3.8), linetype="dashed")+
  geom_vline(xintercept = c(5.7), linetype="dashed")+
  geom_vline(xintercept = c(7), linetype="dashed")+
  geom_smooth(method = "nls", 
              data = subset(Masters, 
                     subset = (Masters$Location == "Bombo (5.0m)") ), 
              se = FALSE,
              aes(y = `Modelled Flow (ped/min/m)`,
                  x = `Space (m2/ped)`), colour = "green",
              formula=y~a*log(x)+k,
              method.args=list(start=c(a=1, k=1)) )+
  geom_smooth(method = "nls", 
              data = subset(Masters, 
                            subset = (Masters$Location == "Jinja (3.5m)") ), 
              se = FALSE,
              aes(y = `Modelled Flow (ped/min/m)`,
                  x = `Space (m2/ped)`), colour = "red",
              formula=y~a*log(x)+k,
              method.args=list(start=c(a=1, k=1)) )+
  geom_smooth(method = "nls", 
              data = subset(Masters, 
                            subset = (Masters$Location == "Entebbe (2.5m)") ), 
              se = FALSE,
              aes(y = `Modelled Flow (ped/min/m)`,
                  x = `Space (m2/ped)`), colour = "purple",
              formula=y~a*log(x)+k,
              method.args=list(start=c(a=1, k=1)) )+
  geom_smooth(method = "nls", 
              data = subset(Masters, 
                            subset = (Masters$Location == "Makerere (2.0m)") ), 
              se = FALSE,
              aes(y = `Modelled Flow (ped/min/m)`,
                  x = `Space (m2/ped)`), colour = "brown",
              formula=y~a*log(x)+k,
              method.args=list(start=c(a=1, k=1)) )



library(ggpubr)

ggplot(data = Masters,
       aes(x = `Space (m2/ped)`,
           y = `Modelled Flow (ped/min/m)`))+ 
  geom_point( colour='blue' )+
  labs(title="Pedestrain Flow Vs Space (inverse Pedestrian Density)",
       x = "Space (m2/ped)",
       y = "Pedestrain Flow (peds/min/m)")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  scale_x_continuous(expand = c(0,0), limits = c(0,7), 
                     breaks = seq(0, 7, by = 0.5)) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 5))+
  geom_vline(xintercept = c(0.8), linetype="dashed")+
  geom_vline(xintercept = c(1.5), linetype="dashed")+
  geom_vline(xintercept = c(2.3), linetype="dashed")+
  geom_vline(xintercept = c(3.8), linetype="dashed")+
  geom_vline(xintercept = c(5.7), linetype="dashed")+
  geom_vline(xintercept = c(7), linetype="dashed")+
  geom_smooth(method = "nls", 
              data = Masters,
              se = FALSE,
              aes(y = `Modelled Flow (ped/min/m)`,
                  x = `Space (m2/ped)`), colour = "green",
              formula=y~a*log(x)+k,
              method.args=list(start=c(a=1, k=1)) )+
  stat_regline_equation(label.y.npc = "top", aes(label = ..eq.label..))+
  stat_regline_equation(label.y = 25, aes(label = ..rr.label..))




ggplot(data = Masters,
       aes(x = `Space (m2/ped)`,
           y = `Modelled Flow (ped/min/m)`))+ 
  geom_point( colour='blue' )+
  labs(title="Pedestrain Flow Vs Space (inverse Pedestrian Density)",
       x = "Space (m2/ped)",
       y = "Pedestrain Flow (peds/min/m)")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  theme(axis.title.x = element_text(size = 8))+
  theme(axis.title.y = element_text(size = 8))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  scale_x_continuous(expand = c(0,0), limits = c(0,7), 
                     breaks = seq(0, 7, by = 0.5)) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 5))+
  geom_vline(xintercept = c(0.8), linetype="dashed")+
  geom_vline(xintercept = c(1.5), linetype="dashed")+
  geom_vline(xintercept = c(2.3), linetype="dashed")+
  geom_vline(xintercept = c(3.8), linetype="dashed")+
  geom_vline(xintercept = c(5.7), linetype="dashed")+
  geom_vline(xintercept = c(7), linetype="dashed")+
  geom_smooth(method = "nls", 
              data = Masters,
              se = FALSE,
              aes(y = `Modelled Flow (ped/min/m)`,
                  x = `Space (m2/ped)`), colour = "green",
              formula=y~a*log(x)+k,
              method.args=list(start=c(a=1, k=1)) )+
  stat_regline_equation(label.y.npc = "top", aes(label = ..eq.label..))

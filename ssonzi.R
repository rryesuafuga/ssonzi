

## install some packages
library("ggplot2")
library("readxl")
library("gridExtra")


## ! begin by setting working directory
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



### input data
Entebbe_Road_Analyzed <- read_excel("Entebbe Road Analyzed.xlsx")

original_names <- colnames(Entebbe_Road_Analyzed)

original_names

colnames(Entebbe_Road_Analyzed) <- c("Density_k", "Flow_q", "Speed")




gridExtra::grid.arrange(
  
  ggplot(Entebbe_Road_Analyzed)+
    geom_smooth(aes(x=Density_k, y=Flow_q), stat = "smooth", color="red",
                position = "identity", method = "lm", 
                formula = y ~ 0 + poly(x, 2), se = FALSE, na.rm = FALSE, 
                show.legend = NA, inherit.aes = TRUE)+
    labs(x="k Pedestrian Density (Peds/m2)", y="q Pedestrian Flow (peds/m/min)",
         title = "k Pedestrian Density (Peds/m2)  Vs 
         q Pedestrian Flow (peds/m/min)"),
  
  ggplot(Entebbe_Road_Analyzed)+
    geom_smooth(aes(x=Flow_q, y=Speed), stat = "smooth", color="red",
                position = "identity", method = "lm", 
                formula = y ~ 0 + poly(x, 2), se = FALSE, na.rm = FALSE, 
                show.legend = NA, inherit.aes = TRUE)+
    labs(x="q Pedestrian Flow (peds/m/min)", y="µ Pedestrian Speed (m/min)",
         title = "q Pedestrian Flow (peds/m/min)  Vs  
         µ Pedestrian Speed (m/min)"),
  
  layout_matrix= matrix(c(1,2), ncol = 2))


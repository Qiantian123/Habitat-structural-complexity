# 1. set working place and loading R packages---------

setwd("D:/R work place/artificial plant")
library(tidyverse)
library(dplyr)
library(glmmTMB)
library(Rmisc)
library(data.table)

########### GLMM ###########

data  <- read.csv("./data/nutrient.csv",header = T) %>%
  filter(Days == 21) %>%
  mutate(ChlTN = ChlTN/1000, ChlTP = ChlTP/1000, Daphniabiomass = Daphniabiomass/1000,
         Cladoceranbiomass = Cladoceranbiomass / 1000, Copepodbiomass = Copepodbiomass / 1000,Rotiferbiomass = Rotiferbiomass / 1000, 
         Zooplanktonbiomass = Zooplanktonbiomass / 1000, Phytoplanktonbiomass = Phytoplanktonbiomass * 0.29, zppp = zppp/1000/0.29) %>%
  mutate(Carpbiomass = log(Carpbiomass+1),
         Carpdensity = log(Carpdensity+1),
         Daphniabiomass = log(Daphniabiomass),
         Cladoceranbiomass = log(Cladoceranbiomass),
         Copepodbiomass = log(Copepodbiomass),
         Rotiferbiomass = log(Rotiferbiomass),
         Zooplanktonbiomass = log(Zooplanktonbiomass),
         Phytoplanktonbiomass = log(Phytoplanktonbiomass),
         zppp = log(zppp),
         Zoopbodymass = log(Zoopbodymass))
head(data)
names(data)
data$Plant <- factor(data$Plant,levels = c("Low","High"),
                     labels = c("Low", "High"))
data$Mandarin <- factor(data$Mandarin,levels = c("Absent","Present"),
                        labels = c("Mandarin absent", "Mandarin present"))


data2 <- gather(data,key = "group",value = "conc",13,15:23)
head(data2)

# calculate GLM predictions
data_a <- list()
data_stat <- list()
index <- count(data2$group)$x

eff <- c("I",
         "Plant",
         "Mandarin",
         "Plant:Mandarin") 

for (i in 1:n_distinct(data2$group)){
  data3 <- filter(data2,group == index[i]) %>% setDT()
  
  # glmm BACI
  fit0 <- glmmTMB(conc ~ Plant * Mandarin + (1|ID),
                  family = gaussian(),
                  data3)
  summary(fit0)
  
  coeff0 <- as.data.frame(summary(fit0)$coefficients$cond) %>% 
    cbind(coeffic = eff) 
  
  data_a[[i]]  <- coeff0 %>% cbind(group = index[i])
  
}

data_stat <- bind_rows(data_a)

names(data_stat)
print(data_stat)

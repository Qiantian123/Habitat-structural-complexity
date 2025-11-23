setwd("D:/R work place/artificial plant")
library(tidyverse)
library(dplyr)
library(glmmTMB)
library(Rmisc)
library(data.table)
########### GLMM ###########
data  <- read.csv("./data/nutrient.csv",header = T) %>%
  filter(Days == 21) %>%
  mutate(ChlTN = ChlTN/1000, ChlTP = ChlTP/1000, Daphnia = Daphnia/1000,
         Cladoceran = Cladoceran / 1000, Copepod = Copepod / 1000,Rotifer = Rotifer / 1000, 
         Total = Total / 1000, Phytoplankton = Phytoplankton * 0.29, zppp = zppp/1000/0.29) %>%
  mutate(Carpbiomass = log(Carpbiomass+1),
         Carpnumber = log(Carpnumber+1),
         Cladoceran = log(Cladoceran),
         Copepod = log(Copepod),
         Rotifer = log(Rotifer),
         Total = log(Total),
         Phytoplankton = log(Phytoplankton),
         zppp = log(zppp),
         Zoopsize = log(Zoopsize))
head(data)
names(data)
data$Plant <- factor(data$Plant,levels = c("Low","High"),
                     labels = c("Low", "High"))
data$Mandarin <- factor(data$Mandarin,levels = c("Absent","Present"),
                        labels = c("Mandarin absent", "Mandarin present"))


data2 <- gather(data,key = "group",value = "conc",18,20:27,38)
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

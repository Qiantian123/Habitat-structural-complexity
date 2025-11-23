setwd("D:/R work place/artificial plant")
library(tidyverse)
library(dplyr)
library(glmmTMB)
library(Rmisc)
library(data.table)
########### GLMM ###########
data  <- read.csv("./data/nutrient.csv",header = T) %>%
  mutate(ChlTN = ChlTN/1000, ChlTP = ChlTP/1000,
         TP = TP*1000, DTP = DTP*1000) %>%
  mutate(OSS = log(OSS),
         TN = log(TN),
         DTN = log(DTN),
         TP = log(TP),
         DTP = log(DTP),
         Chl = log(Chl),
         ChlTN = log(ChlTN),
         ChlTP = log(ChlTP))
head(data)
names(data)
data$Plant <- factor(data$Plant,levels = c("Low","High"),
                     labels = c("Low", "High"))
data$Mandarin <- factor(data$Mandarin,levels = c("Absent","Present"),
                        labels = c("Mandarin absent", "Mandarin present"))


data2 <- gather(data,key = "group",value = "conc",8:13,16,17)
head(data2)

# calculate GLM predictions
data_a <- list()
data_stat <- list()
index <- count(data2$group)$x

eff <- c("I",
         "Plant",
         "Mandarin",
         "T",
         "Plant:Mandarin",
         "Plant:T",
         "Mandarin:T",
         "Plant:Mandarin:T") 

for (i in 1:n_distinct(data2$group)){
  data3 <- filter(data2,group == index[i]) %>% setDT()
  
  # glmm BACI
  fit0 <- glmmTMB(conc ~ Plant * Mandarin * Days + (1|ID),
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
